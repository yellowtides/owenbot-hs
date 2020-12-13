module OwenRegex where

thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: String
thmRE        = ":(thm|theorem) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"         -- :thm
defRE        = "^:(def|definition) *([0-9]{1,2}\\.[0-9]{1,2})"                  -- :def
lemmaRE      = ":(lem|lemma) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"           -- :lemma
textbookRE   = ":textbook *"                                                    -- :textbook
syllogismsRE = ":(syllogisms|syl) *"                                            -- :syllogisms
booleanRE    = ":(boolean|bool) *"                                              -- :boolean
hoogleInfRE  = "^:doc [a-z']+"                                                  -- :doc
helpRE       = "^:helpme *"

owenRegex :: [String] -- list of all regexes 
owenRegex = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]