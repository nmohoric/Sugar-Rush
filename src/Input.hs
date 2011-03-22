module SR.Input
    (InputState) where


data InputState = InputState
    {
        up      :: Bool,
        left    :: Bool,
        down    :: Bool,
        right   :: Bool
     } 
