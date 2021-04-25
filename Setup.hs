import Main(main)

import Distribution.Simple(defaultMainWithHooks, defaultUserHooks)

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
