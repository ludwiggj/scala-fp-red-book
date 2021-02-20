package fpinscala.state

import java.time.LocalDate

// Based on https://medium.com/@wiemzin/immutable-state-in-real-world-e2a3eef2f1b4

case class Education(date: LocalDate, level: String)

case class Experience(date: LocalDate, subject: String)

case class Skill(date: LocalDate, subject: String)

case class Story(birthday: LocalDate,
                 schoolLevel: Option[Education],
                 experiences: Vector[Experience],
                 skills: Vector[Skill])

sealed trait Event

case class GoToSchool(date: LocalDate, level: String) extends Event

case object FinishSchool extends Event

case class AddExperience(date: LocalDate, subject: String) extends Event

case class AddSkill(date: LocalDate, subject: String) extends Event

case class ChangeBirthday(date: LocalDate) extends Event

object CV {
  //TODO: implement the next step in your story
  def makeStory1(oldStory: Story): Story = ???

  val makeStory2: Story => Story = oldStory => ???

  val getAge1: Story => (Int, Story) =
    oldStory =>
      (LocalDate.now.getYear - oldStory.birthday.getYear, ???)

  val getAge2: State[Story, Int] =
    State(oldStory =>
      (LocalDate.now.getYear - oldStory.birthday.getYear, ???))

  def addExperience(experience: Experience): State[Story, Option[Education]] =
    State(oldStory =>
      None -> oldStory.copy(experiences = oldStory.experiences :+ experience)
    )

  def updateEducation(maybeLevel: Option[Education]): State[Story, Option[Education]] =
    State(oldStory =>
      maybeLevel -> oldStory.copy(schoolLevel = maybeLevel)
    )

  def addSkill(skill: Skill): State[Story, Option[Education]] =
    State(oldStory =>
      None -> oldStory.copy(skills = oldStory.skills :+ skill)
    )

  def changeBirthday(newBirthday: LocalDate): State[Story, Option[Education]] = {
    println(s"Birthday changed to $newBirthday")
    State(oldStory =>
      None -> oldStory.copy(birthday = newBirthday)
    )
  }

  def makeStory(event: Event): State[Story, Option[Education]] =
    event match {
      case GoToSchool(date, level) => updateEducation(Some(Education(date, level)))
      case FinishSchool => updateEducation(None)
      case AddExperience(date, subject) => addExperience(Experience(date, subject))
      case AddSkill(date, subject) => addSkill(Skill(date, subject))
      case ChangeBirthday(date) => changeBirthday(date)
    }

  def age(date: LocalDate, birthday: LocalDate): Int =
    date.getYear - birthday.getYear

  def evaluate(scenario: List[Event]): State[Story, (Int, List[Education])] = for {
    education <- State.sequenceTextbook(scenario.map(event => makeStory(event))).map(_.flatten)
    state <- State.get
  } yield (age(LocalDate.now(), state.birthday), education)

  def evaluate2(scenario: List[Event]): State[Story, (Int, List[Education])] = {
    val educationStory: State[Story, List[Education]] = State.sequenceTextbook(scenario.map(event => makeStory(event))).map(_.flatten)
    educationStory.flatMap(
      education => State.get.map(
        state => (age(LocalDate.now(), state.birthday), education)
      )
    )
  }

  def main(args: Array[String]): Unit = {
    val initialStory: Story = Story(LocalDate.of(1990, 9, 22), None, Vector.empty, Vector.empty)

    val scenario: List[Event] = List(
      GoToSchool(LocalDate.of(1995, 9, 5), "Infant School"),
      AddSkill(LocalDate.of(1995, 10, 31), "Painting"),
      AddSkill(LocalDate.of(1996, 6, 13), "Football"),
      GoToSchool(LocalDate.of(1998, 9, 3), "Junior School"),
      AddSkill(LocalDate.of(2003, 4, 14), "Puzzle games"),
      GoToSchool(LocalDate.of(2008, 10, 1), "bachelor's degree"),
      AddExperience(LocalDate.of(2009, 3, 20), "Waiter"),
      AddExperience(LocalDate.of(2010, 7, 12), "Freelancer"),
      AddExperience(LocalDate.of(2011, 2, 16), "Travel to London"),
      FinishSchool,
      ChangeBirthday(LocalDate.of(1970, 4, 14)),
      AddExperience(LocalDate.of(2017, 5, 5), "Get married"),
      AddExperience(LocalDate.of(2018, 10, 10), "Have kids")
    )

    val states: List[State[Story, Option[Education]]] = scenario.map(event => makeStory(event))
    val sequencedState: State[Story, List[Option[Education]]] = State.sequenceTextbook(states)

    println(evaluate(scenario).run(initialStory))
    println(evaluate2(scenario).run(initialStory))
  }
}
