{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MockTags where

--import Apply
import Apply

-- Aa_a
data TAa_a

instance TagC TAa_a a a where
  data TagD TAa_a a a = Aa_a

-- Ab_b
data TAb_b

instance TagC TAb_b c c where
  data TagD TAb_b c c = Ab_b

-- Met_aa
data AMet_aa
