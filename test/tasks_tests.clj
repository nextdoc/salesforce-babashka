(ns tasks-tests
  (:require [clojure.test :refer [deftest is]]
            [tasks :as tasks]
            [babashka.cli :as cli]))

(deftest parsing-cli-arguments
  (is (= {:org-name "scratch1"}
         (cli/parse-opts ["--org-name" "scratch1"] {:spec tasks/cli-args-spec}))))

(deftest checking-responses
  (is (= {:hello "salesforce"}
         (-> {:cli        "bb"
              :command    "fake-sfdx"
              :args       {}
              ; test specific config
              :find-root  identity
              :json-flag? false}
             (tasks/cli-response)
             (tasks/checked-result)))))
