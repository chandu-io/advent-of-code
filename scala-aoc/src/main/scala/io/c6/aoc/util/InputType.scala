package io.c6.aoc.util

sealed trait InputType

object InputType:
  object Sample extends InputType
  object Actual extends InputType
