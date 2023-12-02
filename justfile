# Perform CI pipeline steps
ci:
    clojure -T:build ci

# Use clj-kondo to lint
static:
    clojure -T:build static

# Run application's tests using kaocha test runner
test:
    clojure -T:build test

# Run cljfmt fix
fmt-fix:
    clojure -T:build fmt-fix
