(ns edu.umass.cs.debugger.debug-server
  (:gen-class
    :name edu.umass.cs.debugger/DebugServer)
  (:use [clojure.java.io]
        [clojure.walk]
        [ring.adapter.jetty]
        [ring.util.codec]
        )
  (:import [edu.umass.cs.surveyman.input.csv CSVLexer CSVParser]
           [edu.umass.cs.surveyman.qc Classifier]
           [edu.umass.cs.surveyman.utils Slurpie]
           [java.io StringReader]
           [edu.umass.cs.surveyman.analyses StaticAnalysis DynamicAnalysis])
  )

(def PORT 8001)
(def RESULTS-SUFFIX "_results.csv")
(def classifier Classifier/LOG_LIKELIHOOD)
(defn makeSurvey [url]
  (-> (Slurpie/slurp url) (StringReader.) (CSVLexer.) (CSVParser.) (.parse)))

(def prototypicalitySurvey (makeSurvey "http://surveyman.github.io/SurveyMan/src/test/resources/prototypicality.csv"))
(def prototypicalityStaticAnalysis (StaticAnalysis/staticAnalysis prototypicalitySurvey classifier 100 0.1 0.5))
(def prototypicalityDynamicAnalysis
  (reduce cons [] (pmap (DynamicAnalysis/dynamicAnalysis
                         prototypicalitySurvey
                         (DynamicAnalysis/readSurveyResponses prototypicalitySurvey %)
                         Classifier/LOG_LIKELIHOOD
                         false
                         0.05)
                    (file-seq (file "src/main/resources/results/phonology")))))


(def wageSurvey (makeSurvey "http://surveyman.github.io/SurveyMan/src/test/resources/wage_survey.csv"))
(def wageSurveyStaticAnalysis (StaticAnalysis/staticAnalysis wageSurvey))

(def phonologySurvey (makeSurvey "http://surveyman.github.io/SurveyMan/src/test/resources/phonology.csv"))
(def phonologyStaticAnalysis (StaticAnalysis/staticAnalysis phonologySurvey))

(defn nomen
  [url]
  (first (clojure.string/split (last (clojure.string/split url #"/")) #"\."))
  )

(defn get-content-type-for-request
  [uri]
  (condp = (last (clojure.string/split uri #"\\."))
    "js" "application/javascript"
    "css" "application/css"
    ""
    )
  )

(defn dynamic-analysis
  []
  )

(defn static-analysis
  [{resultsData :resultsData
    surveyDaya :surveyData
    local :local
    survey :survey
    :as data}]
  (println data)
  (if local
    (condp = survey
      "prototypicality"
      )
    )
  )

(defn handle-post
  [uri {s :survey
        local :local
        report :report
        a :survey
        survey-data :surveyData
        survey-results :resultsData
        :as body}]
  (println body)
  (let [local-name (gensym "survey_")
        is-local (read-string local)
        body (if is-local (assoc body :local-survey (str local-name ".csv") :local-results (str local-name RESULTS-SUFFIX)) body)
        ]
    (when is-local
      (println "creating local copy of" local-name)
      (spit (str local-name ".csv") survey-data)
      (println survey-results)
      (when (or survey-results (read-string survey-results))
        (println "creating local copy of" local-name "results")
        (spit (str local-name RESULTS-SUFFIX) survey-results)
        )
      )
    (let [retval (condp = report
                    "static" (static-analysis body)
                    "dynamic" (dynamic-analysis body)
                    (throw (Exception. (str "Unknown report type " report)))
                    )]
      (when (read-string local)
        (println "deleting local copy of" local-name)
        (clojure.java.io/delete-file (str local-name ".csv"))
        (when survey-results
          (println "deleting local copy of" local-name "results")
          (clojure.java.io/delete-file (str local-name RESULTS-SUFFIX))
          )
        )
      retval)
    )
  )

(defn handler [{request-method :request-method
                query-string :query-string
                uri :uri
                params :params
                body :body
                :as request}]
  (println (format "request:%s\tquery:%s\turi:%s\tparams:%s\n" request-method query-string uri params))
  {:status 200
   :headers {"Content-Type" (str (if (= :get request-method) (get-content-type-for-request uri) "text/html")
                              "; charset=utf-8")
             }
   :body (condp = request-method
     :get (Slurpie/slurp (clojure.string/join "" (rest uri)))
     :post (handle-post uri (keywordize-keys (form-decode (slurp body))))
    )
   }
  )

(defn -main
  [& args]
  (ring.adapter.jetty/run-jetty
    handler
    {:port PORT :join? false})
  )

