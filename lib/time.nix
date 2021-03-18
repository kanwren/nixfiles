rec {
  nano = x: x;
  micro = x: 1000 * x;
  millis = x: 1000 * x;
  seconds = x: 1000 * millis x;
  minutes = x: 60 * seconds x;
  hours = x: 60 * minutes x;
  days = x: 24 * hours x;
  weeks = x: 7 * days x;
  years = x: 365 * days x;
  units = u: num: num / u 1;
}
