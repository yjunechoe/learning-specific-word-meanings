resolveSallyMessage = function(label, labelled = true, first = true) {
  let directions
  if (labelled) {
    directions = ", these are " + label + "s!<br>Do you see the " + label + "s?"
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
