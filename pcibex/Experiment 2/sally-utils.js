resolveName = function(label, number, first = TRUE) {
  let directions
  if (number === "one") {
    directions = ", this is a " + label + "!<br>Do you see the " + label + "?"
  } else if (number === "three") {
    directions = ", these are " + label + "s!<br>Do you see the " + label + "s?"
  }
  if (first) {
    directions = "Look" + directions
  } else {
    directions = "And look" + directions
  }
  return directions
}