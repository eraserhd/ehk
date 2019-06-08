module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth   = (/ 31557600)
ageOn Mercury = (/   0.24084670) . ageOn Earth
ageOn Venus   = (/   0.61519726) . ageOn Earth
ageOn Mars    = (/   1.88081580) . ageOn Earth
ageOn Jupiter = (/  11.86261500) . ageOn Earth
ageOn Saturn  = (/  29.44749800) . ageOn Earth
ageOn Uranus  = (/  84.01684600) . ageOn Earth
ageOn Neptune = (/ 164.79132000) . ageOn Earth
