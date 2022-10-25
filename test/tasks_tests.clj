(ns tasks-tests
  (:require [clojure.test :refer [deftest is testing]]
            [tasks :as tasks]
            [babashka.cli :as cli]))

(deftest parsing-cli-arguments
  (is (= {:org-name "scratch1"}
         (cli/parse-opts ["--org-name" "scratch1"] {:spec tasks/cli-args-spec}))))

(deftest checking-responses
  (is (= {:hello "salesforce"}
         (-> {:cli        "bb"
              :command    "fake:sfdx"
              :args       {}
              ; test specific config
              :find-root  identity
              :json-flag? false}
             (tasks/cli-response)
             (tasks/checked-result)))))

(deftest polling-package-install
  (testing "single poll result shape"
    (is (= {:status "IN_PROGRESS"}
           (-> {:cli        "bb"
                :command    "fake:sf-install-report-incomplete"
                :args       {:request-id "0HfN00000009VvbKAE"}
                ; test specific config
                :find-root  identity
                :json-flag? false}
               (tasks/cli-response)
               (tasks/checked-result)))))
  (testing "polling loop success result shape"
    (is (= {:response   {:status "SUCCESS"}
            :poll-count 2}
           (-> {:command        (fn [{:keys [poll-count]}]
                                  {:cli        "bb"
                                   :command    (if (= 2 poll-count)
                                                 "fake:sf-install-report-success"
                                                 "fake:sf-install-report-incomplete")
                                   :args       {:request-id "0HfN00000009VvbKAE"}
                                   ; test specific config
                                   :find-root  identity
                                   :json-flag? false})
                :success?       (fn [result]
                                  (= "SUCCESS" (:status result)))
                :poll-max       3
                :sleep-duration 1}
               (tasks/poll-for-success!)
               (tasks/checked-result)))))
  (testing "polling loop timeout"
    ; avoid using tasks/checked-result for failures to avoid throwing an exception
    (is (= {:status  1
            :message "maximum polls reached"}
           (-> {:command        (fn [_]
                                  {:cli        "bb"
                                   :command    "fake:sf-install-report-incomplete"
                                   :args       {:request-id "0HfN00000009VvbKAE"}
                                   ; test specific config
                                   :find-root  identity
                                   :json-flag? false})
                :success?       (fn [result]
                                  (= "SUCCESS" (:status result)))
                :poll-max       1
                :sleep-duration 1}
               (tasks/poll-for-success!))))))

(comment

  ; emulate terminal output to check coloring
  (-> {:cli        "bb"
       :command    "fake:sf-deploy"
       :args       {}
       ; test specific config
       :find-root  identity
       :json-flag? false}
      (tasks/cli-response)
      (tasks/checked-result)
      (tasks/report-deploy-counts)
      )

  )
