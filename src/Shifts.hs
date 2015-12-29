module Shifts where

data Position = Position 
    { files :: [ FilePath ] -- List of files
    , ix_shuffle :: [ Int ] -- Shuffle of indexes
    , ix_pos :: Int         -- Index of file to display. Refers to `files`
    , ix_rand :: Int        -- Current position in random list. Refers to `ix_shuffle`
    , mask :: [ Bool ]      -- Mask of file correctness
    }

{- Function which determines which file will be next -}
type Shift = Position -> Position

{- Next Random -}
nextRan :: Shift
nextRan pos = pos { ix_rand = foo
                  , ix_pos = ix_shuffle pos !! safeIndex
                  }
    where foo = ( ix_rand pos ) + 1
          safeIndex = ( mod foo ( length.files $ pos ))

{- Previous Random -}
prevRan :: Shift
prevRan pos = pos { ix_rand = foo
                  , ix_pos = ix_shuffle pos !! safeIndex
                  }
        where foo = ( ix_rand pos ) - 1
              safeIndex = ( mod foo ( length.files $ pos ))

{- Next Sequential -}
nextSeq :: Shift
nextSeq pos = pos { ix_pos = ( ix_pos pos ) + 1 }

{- Previous Sequential -}
prevSeq :: Shift
prevSeq pos = pos { ix_pos = ( ix_pos pos ) - 1 }
