package io.c6.aoc

private enum InputType(private val sampleInput: Boolean, private val part2Input: Boolean):
  // Sample Input for Part 1
  case S1 extends InputType(true, false)
  // Actual Input for Part 1
  case A1 extends InputType(false, false)
  // Sample Input for Part 2
  case S2 extends InputType(true, true)
  // Actual Input for Part 2
  case A2 extends InputType(false, true)
