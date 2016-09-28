# early-slp-bias
Adjustments to SLP to remove the systematic bias in ICOADS3 pre-1870

Pressure observations in ICOADS are systematically biased low before about 1860. This bias produces large artefacts in datasets that use these obs - including 20CR.

We don't know for sure what causes the bias, but it looks as if it varies substantially from ship to ship but does not change systematically with environmental variables like temperature. That suggests it's an instrument problem rather than a physical effect or mis-applied correction, so it's sensible to address it by applying ship-specific offsets for each affected ship.
