object phone
  import scala.io.StdIn
  import java.io.{File, PrintWriter}

  // Define a case class to represent an entry in the telephone directory
  case class Entry(name: String, phoneNumber: String)
  // Define a class hierarchy using traits
  object TelephoneDirectory extends App {
    // Define a trait for database operations
    trait Database {
      def readFromFile(filename: String): List[Entry]
      def saveToFile(filename: String, data: List[Entry]): Unit
    }

    // Define a trait for interactive operations
    trait Interactive {
      def addEntry(entries: List[Entry]): List[Entry]
      def findPhone(entries: List[Entry]): Unit
      def findName(entries: List[Entry]): Unit
      def showContents(entries: List[Entry]): Unit
    }

    // Implement the Database trait
    trait FileDatabase extends Database {
      def readFromFile(filename: String): List[Entry] = {
        try {
          val source = scala.io.Source.fromFile(filename)
          val entries = source.getLines().map { line =>
            val Array(name, phoneNumber) = line.split(",")
            Entry(name, phoneNumber)
          }.toList
          source.close()
          entries
        } catch {
          case _: Throwable => List.empty[Entry]
        }
      }

      def saveToFile(filename: String, data: List[Entry]): Unit = {
        val writer = new PrintWriter(new File(filename))
        data.foreach(entry => writer.println(s"${entry.name},${entry.phoneNumber}"))
        writer.close()
      }
    }

    // Implement the Interactive trait
    trait TelephoneInterface extends Interactive {
      def addEntry(entries: List[Entry]): List[Entry] = {
        println("Enter name:")
        val name = StdIn.readLine()
        println("Enter phone number:")
        val phoneNumber = StdIn.readLine()
        Entry(name, phoneNumber) :: entries
      }

      def findPhone(entries: List[Entry]): Unit = {
        println("Enter name to find phone number:")
        val name = StdIn.readLine()
        entries.find(_.name == name) match {
          case Some(entry) => println(s"Phone number for $name: ${entry.phoneNumber}")
          case None => println(s"No entry found for $name")
        }
      }

      def findName(entries: List[Entry]): Unit = {
        println("Enter phone number to find name:")
        val phoneNumber = StdIn.readLine()
        entries.find(_.phoneNumber == phoneNumber) match {
          case Some(entry) => println(s"Name for phone number $phoneNumber: ${entry.name}")
          case None => println(s"No entry found for phone number $phoneNumber")
        }
      }

      def showContents(entries: List[Entry]): Unit = {
        println("Telephone Directory:")
        entries.foreach(entry => println(s"${entry.name}: ${entry.phoneNumber}"))
      }
    }

    // Combine traits to create a concrete class
    class TelephoneDirectory extends FileDatabase with TelephoneInterface

    // Start the program
    val filename = "telephone_directory.txt"
    val telephoneDirectory = new TelephoneDirectory
    var entries = telephoneDirectory.readFromFile(filename)

    var continue = true
    while (continue) {
      println("\nChoose an option:")
      println("0 - Exit")
      println("1 - Add an entry")
      println("2 - Find a phone by name")
      println("3 - Find a name by phone")
      println("4 - Save current data to file")
      println("5 - Show contents of the entire phone book")

      val option = StdIn.readInt()
      option match {
        case 0 => continue = false
        case 1 => entries = telephoneDirectory.addEntry(entries)
        case 2 => telephoneDirectory.findPhone(entries)
        case 3 => telephoneDirectory.findName(entries)
        case 4 => telephoneDirectory.saveToFile(filename, entries)
        case 5 => telephoneDirectory.showContents(entries)
        case _ => println("Invalid option. Please choose again.")
      }
    }

}
