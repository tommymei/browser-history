(ns history-test
  (:require [clojure.test :refer :all]
            [history :refer :all]))

(deftest create-history-test
  (testing "test create history : zero"
    (is (thrown? AssertionError (create-history 0))))

  (testing "test create history : negative"
    (is (thrown? AssertionError (create-history -1))))

  (testing "test create history : positive"
    (is (= {:max 10, :index nil, :entries nil}
           (create-history 10)))))


(deftest back-test
  (testing "test back with no entries"
    (is (= {:max 10, :index nil, :entries nil}
           (-> (create-history 10)
               back))))

  (testing "test back with one entries"
    (is (= {:max 10, :index 0, :entries ["http://www.google.com"]}
           (-> (new-history 10 0 ["http://www.google.com"])
               back))))

  (testing "test back with two entries"
    (is (= {:max 10, :index 0, :entries ["http://www.google.com"
                                               "http://www.yahoo.com"]}
           (-> (new-history 10 1 ["http://www.google.com"
                                  "http://www.yahoo.com"])
               back)))))

(deftest fwd-test
  (testing "Test forward with no entries"
    (is (= {:max 10, :index nil, :entries nil}
           (-> (create-history 10)
               fwd))))

  (testing "test forward with one entries"
    (is (= {:max 10, :index 0, :entries ["http://www.google.com"]}
           (-> (new-history 10 0 ["http://www.google.com"])
               fwd))))

  (testing "test forward with two entries"
    (is (= {:max 10, :index 1, :entries ["http://www.google.com"
                                               "http://www.yahoo.com"]}
           (-> (new-history 10 0 ["http://www.google.com"
                                  "http://www.yahoo.com"])
               fwd)))))


(deftest visit-test
  (testing "Visit on empty history"
    (is (= {:max 10, :index 0, :entries ["http://www.google.com"]}
           (-> (create-history 10)
               (visit "http://www.google.com")))))

  (testing "Visit on one entry history"
    (is (= {:max 10, :index 1, :entries ["http://www.google.com"
                                               "http://www.yahoo.com"]}
           (-> (new-history 10 0 ["http://www.google.com"])
               (visit "http://www.yahoo.com")))))

  (testing "Visit on history where you are at the end of list"
    (is (= {:max 10, :index 2, :entries ["http://www.google.com"
                                               "http://www.yahoo.com"
                                               "http://www.microsoft.com"]}
           (-> (new-history 10 1 ["http://www.google.com"
                                  "http://www.yahoo.com"])
               (visit "http://www.microsoft.com")))))

  (testing "Visit on history where you are NOT at end of list"
    (is (= {:max 10, :index 1, :entries ["http://www.google.com"
                                               "http://www.microsoft.com"]}
           (-> (new-history 10 0 ["http://www.google.com"
                                  "http://www.yahoo.com"])
               (visit "http://www.microsoft.com")))))

  (testing "Visit on history where there are max entries and you are at the end of list"
    (is (= {:max 2, :index 1, :entries ["http://www.yahoo.com"
                                              "http://www.microsoft.com"]}
           (-> (new-history 2 1 ["http://www.google.com"
                                 "http://www.yahoo.com"])
               (visit "http://www.microsoft.com")))))

  (testing "Visit on history where there are max entries and you are NOT end of list"
    (is (= {:max 2, :index 1, :entries ["http://www.google.com"
                                              "http://www.microsoft.com"]}
           (-> (new-history 2 0 ["http://www.google.com"
                                 "http://www.yahoo.com"])
               (visit "http://www.microsoft.com"))))))


(deftest lookup-test
  (testing "test match"
    (is (= ["http://www.google.com"]
           (lookup (new-history 2 0 ["http://www.google.com"
                                     "http://www.yahoo.com"]) "goo"))))

  (testing "test match with multiple results"
    (is (= ["http://www.google.com"
            "http://www.yahoo.com"]
           (lookup (new-history 2 0 ["http://www.google.com"
                                     "http://www.yahoo.com"]) "com"))))
  (testing "test NO match"
    (is (= nil (lookup (new-history 2 0 ["http://www.google.com"
                                         "http://www.yahoo.com"]) "blah")))))


