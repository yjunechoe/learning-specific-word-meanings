resolveName = function(label, plural = true, first = true) {
  let directions
  if (plural) {
    directions = ", these are " + label + "s!<br>Do you see the " + label + "s?"
  } else {
    directions = ", this is a " + label + "!<br>Do you see the " + label + "?"
  }
  if (first) {
    directions = "Look" + directions
  } else {
    directions = "And look" + directions
  }
  return directions
}