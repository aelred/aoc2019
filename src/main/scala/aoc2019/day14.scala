package aoc2019

import aoc2019.parser.~
import aoc2019.parser.Parser
import aoc2019.parser.Parser._

import scala.collection.mutable

object day14 {

  object Solution extends Solution[Seq[Reaction]] {
    def part1: Long = Reaction.oreForFuel(input, 1)
    def part2: Long = binarySearch(1000000000000L, Reaction.oreForFuel(input, _), 1, Int.MaxValue)
  }

  private def binarySearch(expected: Long, op: Long => Long, min: Long = Long.MinValue, max: Long = Long.MaxValue): Long = {
    val value = (max + min) / 2
    val result = op(value)

    if (result == expected || value == min) {
      value
    } else if (result > expected) {
      binarySearch(expected, op, min, value)
    } else {
      binarySearch(expected, op, value, max)
    }
  }

  case class Material(name: String, amount: Long) {
    def *(value: Long): Material = copy(amount=amount*value)

    override def toString: String = s"$name $amount"
  }

  object Material {
    val parser: Parser[Material] = int ~ " " ~ word >> { case amount ~ _ ~ name => Material(name, amount)
    }
  }

  case class Reaction(inputs: Seq[Material], output: Material) {

    def *(value: Long): Reaction = {
      Reaction(inputs.map(_ * value), output * value)
    }

    override def toString: String = s"${inputs.mkString(", ")} => $output"
  }

  object Reaction {
    implicit val parser: Parser[Reaction] = Material.parser.separatedBy(", ") ~ " => " ~ Material.parser >> {
      case inputs ~ _ ~ output => Reaction(inputs, output)
    }

    def react(reactions: Seq[Reaction], initialMaterials: Seq[Material]): Seq[Material] = {

      val reactionForMaterial = reactions.map(r => r.output.name -> r).toMap

      val materials = mutable.Map.from(initialMaterials.map(m => m.name -> m.amount)).withDefaultValue(0L)

      def materialsSeq = {
        materials.toSeq
          .map{case (name, amount) => Material(name, amount)}
      }

      def neededMaterials = materialsSeq.filter(m => m.name != "ORE" && m.amount < 0)

      while (neededMaterials.nonEmpty) {
        val material = neededMaterials.head

        reactionForMaterial.get(material.name) foreach { reaction =>
          val scaledReaction = reaction * (1L + (-material.amount - 1) / reaction.output.amount)

          for (in <- scaledReaction.inputs) {
            materials(in.name) -= in.amount
          }
          val output = scaledReaction.output
          materials(output.name) += output.amount
        }
      }

      materialsSeq
    }

    def oreForFuel(reactions: Seq[Reaction], fuel: Long): Long = {
      val materials = react(reactions, Seq(Material("FUEL", -fuel)))
      -materials.find(_.name == "ORE").get.amount
    }
  }
}
