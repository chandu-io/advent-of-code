package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.BaseSolution.*
import io.c6.aoc.Day.*
import io.c6.aoc.InputType.*
import io.c6.aoc.Year.*

import scala.collection.mutable

object Day07 extends BaseSolution:
  override protected def part1InputFileName: String = getInputFileName(_2022, _07, A1)

  private val slash = "/"

  private sealed trait FileEntry {
    def name: String

    def size: Long
  }

  private class Dir(val name: String, val parent: Option[Dir]) extends FileEntry {
    private val _children: mutable.Set[FileEntry] = mutable.LinkedHashSet()

    def children: Set[FileEntry] = _children.toSet

    def size: Long = _children.map(_.size).sum

    def add(entry: FileEntry): Unit = _children += entry

    override def toString: String = parent.map(_.toString)
      .map { s => if (s == slash) s + name else s + slash + name }
      .getOrElse(slash)

    override def equals(obj: Any): Boolean = s"$obj" == toString

    override def hashCode(): Int = toString.hashCode
  }

  private case class File(name: String, size: Long, parent: Dir) extends FileEntry {
    override def toString: String = parent.toString + slash + name
  }

  private class CommandExecutor {
    val root: Dir = Dir(slash, None)
    private var pwd: Dir = root
    private val uniqueDirs: mutable.Set[Dir] = mutable.LinkedHashSet()

    def execute(command: String): Unit =
      command.split(" ") match
        case Array("$", "cd", `slash`) =>
          pwd = root
        case Array("$", "cd", "..") =>
          pwd = pwd.parent.getOrElse(root)
        case Array("$", "cd", dirName) =>
          pwd.children.find(e => e.name == dirName && e.isInstanceOf[Dir]).foreach { e => pwd = e.asInstanceOf[Dir] }
        case Array("$", "ls") =>
          ()
        case Array("dir", dirName) =>
          pwd.add(Dir(dirName, Some(pwd)))
        case Array(sizeStr, fileName) =>
          pwd.add(File(fileName, sizeStr.toLong, pwd))
      uniqueDirs += pwd

    def allDirs: Set[Dir] = uniqueDirs.toSet
  }

  override protected def part1Solution: Seq[String] => Unit = { input =>
    val executor = new CommandExecutor
    input.foreach(executor.execute)
    val result = executor.allDirs.toSeq.map(_.size).filter(_ <= 100_000).sum
    println(s"Result for part 1: $result")
  }

  override protected def part2Solution: Seq[String] => Unit = { input =>
    val executor = new CommandExecutor
    input.foreach(executor.execute)
    val totalSpace = 70_000_000
    val requiredFreeSpace = 30_000_000
    val usedSpace = executor.root.size
    val remainingSpace = totalSpace - usedSpace
    val removableSpace = requiredFreeSpace - remainingSpace
    val result = executor.allDirs.toSeq.map(_.size).filter(_ > removableSpace).min
    println(s"Result for part 2: $result")
  }

@main def runDay07: Unit = Day07.run
