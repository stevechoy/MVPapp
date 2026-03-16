# Generate Dosing Regimens

This is the main function to supply dosing regimens to simulations. If
there are no valid dose amounts from any dosing regimens, a dummy
mrgsolve::ev() object is created to supply a dose of 0 to be used for
simulations. Note that weight-based dosing is handled later inside
transform_ev_df()

## Usage

``` r
generate_dosing_regimens(
  amt1,
  delay_time1,
  cmt1,
  tinf1,
  total1,
  ii1,
  amt2,
  delay_time2,
  cmt2,
  tinf2,
  total2,
  ii2,
  amt3,
  delay_time3,
  cmt3,
  tinf3,
  total3,
  ii3,
  amt4,
  delay_time4,
  cmt4,
  tinf4,
  total4,
  ii4,
  amt5,
  delay_time5,
  cmt5,
  tinf5,
  total5,
  ii5,
  mw_conversion = 1,
  create_dummy_ev = TRUE,
  debug = FALSE
)
```

## Arguments

- amt1:

  Dose Amount Regimen 1

- delay_time1:

  Delay Time Regimen 1

- cmt1:

  Input CMT Regimen 1

- tinf1:

  Infusion Time Regimen 1

- total1:

  Total Doses Regimen 1

- ii1:

  Interdose Interval Regimen 1

- amt2:

  Dose Amount Regimen 2

- delay_time2:

  Delay Time Regimen 2

- cmt2:

  Input CMT Regimen 2

- tinf2:

  Infusion Time Regimen 2

- total2:

  Total Doses Regimen 2

- ii2:

  Interdose Interval Regimen 2

- amt3:

  Dose Amount Regimen 3

- delay_time3:

  Delay Time Regimen 3

- cmt3:

  Input CMT Regimen 3

- tinf3:

  Infusion Time Regimen 3

- total3:

  Total Doses Regimen 3

- ii3:

  Interdose Interval Regimen 3

- amt4:

  Dose Amount Regimen 4

- delay_time4:

  Delay Time Regimen 4

- cmt4:

  Input CMT Regimen 4

- tinf4:

  Infusion Time Regimen 4

- total4:

  Total Doses Regimen 4

- ii4:

  Interdose Interval Regimen 4

- amt5:

  Dose Amount Regimen 5

- delay_time5:

  Delay Time Regimen 5

- cmt5:

  Input CMT Regimen 5

- tinf5:

  Infusion Time Regimen 5

- total5:

  Total Doses Regimen 5

- ii5:

  Interdose Interval Regimen 5

- mw_conversion:

  MW conversion value

- create_dummy_ev:

  Default TRUE, to create dummy ev if there are no valid dose amounts,
  set to FALSE to not create one

- debug:

  set to TRUE to show debug messages

## Value

A mrgsolve::ev event object
