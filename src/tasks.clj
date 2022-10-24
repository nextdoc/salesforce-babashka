(ns tasks
  (:require
    [babashka.cli :as cli]
    [babashka.fs :as fs]
    [babashka.process :refer [process]]
    [cheshire.core :as json]
    [clojure.string :as str]
    [zprint.core :as z])
  (:import (java.io InputStream)))

(def cli-args-spec
  {:org-name {:ref      "<scratch org alias>"
              :desc     "The alias used when creating the new scratch org. must be at least 3 chararacters"
              :require  true
              :validate (fn [s]
                          (and (string? s)
                               (>= (count s) 3)))}})

(defn parse-dx-cli-options
  "Used as first task in bb cli for parsing args.
   Uses tasks/cli-args-spec to control argument parsing."
  {:org.babashka/cli {:spec     cli-args-spec
                      :error-fn (fn [{:keys [spec type cause msg option] :as data}]
                                  (if (= :org.babashka/cli type)
                                    (case cause
                                      :require
                                      (println
                                        (format "Missing required argument:\n%s"
                                                (cli/format-opts {:spec (select-keys spec [option])})))
                                      (println msg))
                                    (throw (ex-info msg data)))
                                  (System/exit 1))}}
  [m]
  m)

(defn check
  "Takes a process, waits until is finished and throws if exit code is non-zero.
   Adapted from process/check to :out to provide error message before :err"
  [proc]
  (let [proc @proc
        exit-code (:exit proc)
        {:keys [err out]} proc]
    (if (not (zero? exit-code))
      (let [err (cond
                  (string? out)
                  out

                  (string? err)
                  err

                  (instance? InputStream err)
                  (slurp err)

                  :else
                  nil)]
        (throw (ex-info (if (string? err)
                          err
                          "failed")
                        (assoc proc :type ::error))))
      proc)))

(defn dx-project-directory?
  "Returns true when the arg is the root dir for a source tracked SFDC project.
   Assumes project dir has a config/project-scratch-def.json file."
  [f]
  (and
    (fs/directory? f)
    (not (empty? (fs/match f "glob:config/project-scratch-def.json" {:recursive true})))))

(defn dx-project-root
  "Search current dir and its ancestors looking for a dx project root.
   Returns nil if not root found."
  [start]
  (loop [f start]
    (cond
      (nil? f) f
      (= "/" (str f)) nil
      (dx-project-directory? f) f
      :else (recur (fs/parent f)))))

(defn cli-args
  "transform a map of clojure keywords into a cli args string"
  [args-map {:keys [remove-dashes?]
             :or   {remove-dashes? true}}]
  (reduce-kv (fn [args arg-key value]
               (str args
                    " --" (cond-> (name arg-key)
                                  remove-dashes? (str/replace "-" ""))
                    (if (true? value)
                      ""
                      (str " " value))))
             ""
             args-map))

(defn try-json-parse
  "attempt to parse a JSON string, returning a map instead of an exception if the parse fails"
  [s]
  (try
    (json/parse-string s true)
    (catch Throwable _
      {:status   1
       :response s})))

(defn cli-response
  "Invoke a CLI process. returns the response as parsed JSON.
   If response is not parsable as JSON, it is returned as a string in a fail response."
  [{:keys [cli command args find-root json-flag?]
    :or   {find-root  dx-project-root
           json-flag? true}}]
  (if-let [root (find-root (fs/cwd))]
    (let [args-string (cond-> args
                              json-flag? (assoc :json true)
                              :always (cli-args {:remove-dashes? (= "sfdx" cli)}))
          request (str cli " " command " " args-string)
          _ (println "    " request)
          response (-> request
                       (process {:dir (str root)
                                 :out :string})
                       check)]
      (try-json-parse (:out response)))
    (throw (ex-info "no DX root found" {:start (str (fs/cwd))}))))

(defn checked-result
  "Return the result of a response if it was successful.
   Throw an exception if not."
  [response]
  (when-not (zero? (:status response))
    (-> response :response :out println)
    (throw (Exception. "CLI command failed")))
  (:result response))

(z/set-options! {:map       {:comma? false}
                 :color-map {:string  :bright-purple
                             :number  :bright-green
                             :bracket :bright-yellow
                             :paren   :bright-yellow
                             :brace   :bright-yellow}})

(defn report-deploy-counts
  [result]
  (->> result
       :files
       (map :type)
       frequencies
       (into (sorted-map))
       (vector "Successful deployment of...")
       (z/czprint))
  result)

(defn handle-deploy-exception
  "'sf deploy metadata' specific exception handler.
   Extracts and logs the failing source files to make finding/fixing deploys easier (less verbose)"
  [e]
  (throw (or (when-let [files (some->> (ex-data e)
                                       :out
                                       try-json-parse
                                       :result :files
                                       (remove #(contains? #{"Changed" "Unchanged"} (:state %)))
                                       set
                                       (sort-by :fullName)
                                       vec)]
               (clojure.pprint/pprint {:failing-files files})
               (ex-info "not all files deployed ok" {:failing-files files}))
             e)))

(defn deploy-metadata!
  "Use the sf deploy metadata command to load source from a dir into the default scratch org.
   On success, report a summary of the results.
   On failure, report a list of the failed source files."
  [dir]
  (try
    (-> {:cli     "sf"
         :command "deploy metadata"
         :args    {:source-dir dir}}
        (tasks/cli-response)
        (tasks/checked-result)
        (tasks/report-deploy-counts))
    (catch Exception e
      (tasks/handle-deploy-exception e))))
