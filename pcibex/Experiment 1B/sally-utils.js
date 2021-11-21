resolveSallyMessage = function(label, first = true) {
  let directions
  const labelled = label !== ""
  if (labelled) {
    directions = ", this is a " + label + "!<br>Do you see the " + label + "?"
  } else {
    directions = " over here!<br>Do you see this?"
  }
  if (first) {
    directions = "Look" + directions
  } else {
    directions = "And look" + directions
  }
  return directions
}
