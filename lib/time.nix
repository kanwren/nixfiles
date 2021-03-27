rec {
  nano = x: x;
  micro = x: 1000 * nano x;
  millis = x: 1000 * micro x;
  seconds = x: 1000 * millis x;
  minutes = x: 60 * seconds x;
  hours = x: 60 * minutes x;
  days = x: 24 * hours x;
  weeks = x: 7 * days x;
  years = x: 365 * days x;

  # Given a time unit and a time quantity, output a quantity in that unit
  #
  # Example:
  #   unitsOf minutes (years 1)
  #   ==> 525600
  unitsOf = u: num: num / u 1;
}
