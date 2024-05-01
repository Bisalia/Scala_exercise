package phone_directory

object phone_directory extends App {
  import scala.io.StdIn
  import java.io.{File, PrintWriter}
  import scala.util.{Try, Success, Failure}

    // Define Directory outside functions to make it globally accessible
    type Directory = Map[String, String]
    var directory: Directory = loadDirectory()

    // Load directory from a file if it exists, otherwise initialize a new directory
    def loadDirectory(): Directory = {
      val file = new File("directory.txt")
      if (file.exists()) {
        val lines = scala.io.Source.fromFile(file).getLines().toList
        lines.map(_.split(",")).map(arr => (arr(0) -> arr(1))).toMap
      } else {
        Map.empty[String, String]
      }
    }

    // Main function of the program
    def main(): Unit = {
      var continue = true
      while (continue) {
        println("Choose an option:")
        println("0 - Quit:")
        println("1 - Add an entry (name and phone number):")
        println("2 - Search for a phone by name:")
        println("3 - Search for a name by phone number:")
        println("4 - Save current data to a file")
        println("5 - Display the entire phone directory")
        val choice = Try(StdIn.readInt())
        choice match {
          case Success(0) =>
            println("Thank you for using the phone directory.")
            continue = false
          case Success(1) =>
            println("Enter the name:")
            val name = StdIn.readLine()
            println("Enter the phone number:")
            val phoneNumber = StdIn.readLine()
            directory += (name -> phoneNumber)
            println("Entry added successfully.")
          case Success(2) =>
            println("Enter the name to search for the phone number:")
            val name = StdIn.readLine()
            directory.get(name) match {
              case Some(phoneNumber) => println(s"The phone number of $name is $phoneNumber.")
              case None => println(s"No phone number found for $name.")
            }
          case Success(3) =>
            println("Enter the phone number to search for the name:")
            val phoneNumber = StdIn.readLine()
            val result = directory.find { case (_, num) => num == phoneNumber }
            result match {
              case Some((name, _)) => println(s"The name corresponding to the phone number $phoneNumber is $name.")
              case None => println(s"No name found for the phone number $phoneNumber.")
            }
          case Success(4) =>
            saveDirectory(directory)
            println("Data saved successfully to the file.")
          case Success(5) =>
            println("Content of the entire phone directory:")
            directory.foreach { case (name, phoneNumber) => println(s"$name : $phoneNumber") }
          case _ =>
            println("Invalid option. Please enter a number corresponding to one of the options.")
        }
      }
    }

    // Save directory to a file
    def saveDirectory(directory: Directory): Unit = {
      val writer = new PrintWriter(new File("directory.txt"))
      directory.foreach { case (name, phoneNumber) => writer.println(s"$name,$phoneNumber") }
      writer.close()
    }

    // Launch the program
    main()
  }


