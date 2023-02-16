import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.util.control.Breaks._

// TODOS:
// - ลองเช็คเรื่อง Level of Abstraction และการตั้งชื่อฟังก์ชัน สะท้อนกับการใช้งานจริง หรือสื่อความหมาย ตรงกับ value ที่ได้ไหม
// - Data model ในหลายส่วน ยังแปลกๆ -- ต้อง Specify เพิ่ม

case class Movie(title: String)

// NOTES: 
// - Convention ที่ใช้การตั้งชื่อ field ว่า id คืออะไร และจะแยกยังไง ถ้า Model ใกล้กัน แต่ใช้คนละ Type
// - อาจจะใช้คำว่า code แทน id
// - คำว่า title ในบริบทนี้ ยังกำกวม และดูแปลก
case class MovieLanguage(id: String, title: String)

case class Cinema(id: Int)

case class Showtime(startAt: LocalDateTime, endAt: LocalDateTime)

case class MovieShowtime(
    movie: Movie,
    language: MovieLanguage,
    cinema: Cinema,
    showtime: Showtime
)

object MovieShowtime {
    def generateMovieShowListMock: List[MovieShowtime] =
        List(
            MovieShowtime(
                Movie("Avatar 2"),
                MovieLanguage("TH", "Thai"),
                Cinema(1),
                Showtime(
                    LocalDateTime.parse("2023-01-25T18:15:00"),
                    LocalDateTime.parse("2023-01-25T20:15:00")
                )
            ),
            MovieShowtime(
                Movie("Avatar 2"),
                MovieLanguage("EN", "Soundtrack (English)"),
                Cinema(2),
                Showtime(
                    LocalDateTime.parse("2023-01-25T18:15:00"),
                    LocalDateTime.parse("2023-01-25T20:15:00")
                )
            ),
            MovieShowtime(
                Movie("M3GAN"),
                MovieLanguage("EN", "Soundtrack (English)"),
                Cinema(3),
                Showtime(
                    LocalDateTime.parse("2023-01-25T18:15:00"),
                    LocalDateTime.parse("2023-01-25T20:15:00")
                )
            )
        )

        // TODOS:
        // - แก้ชื่อฟังก์ชัน ตามชื่อ Model ที่เปลี่ยนไป
        // NOTE:
        // - ชื่อฟังก์ชัน ไม่สอดคล้องกับ Return type -- จะรู้ได้ไง ถ้าอ่านจากชื่อฟังก์ชั่น ว่ามัน Return MovieSHowSelectionList
        // - คำว่า option ดูกำกวมมาก
        // - ฟังก์ชั่นนี้เอาไว้ทำอะไรได้บ้าง
        def generateMovieShowtimeSelectionList(movieShowtimeList: List[MovieShowtime]): List[(Int, String)] = {
            val options = movieShowtimeList.zipWithIndex
            
            options.map { case (option, index) => (index + 1, MovieShowtimePresenter.formatMenuItem(option)) }
        }

        // TODOS:
        // - แก้ชื่อฟังก์ชัน ตามชื่อ Model ที่เปลี่ยนไป
        // - Function บรรทัดเดียว ไม่ต้องใช้ { }
        // NOTES:
        // - คำว่า Selectable กำกวมมาก (ทำไมใช้คำนี้ ต่างจาก Selection ยังไง)
        // - ฟังก์ชันนี้ อาจจะไม่จำเป็น
        def generateMovieShowtimeSelectableSet(movieShowtimeSelectionList: List[(Int, String)]): Set[String] =
            movieShowtimeSelectionList.map { case (index, item) => index.toString }.toSet

}

object MovieShowtimePresenter {
    def formatMenuItem(movieShowtime: MovieShowtime): String =
        s"${movieShowtime.movie.title} " +
          s"(${movieShowtime.language.title}) " +
          s"show start at " +
          s"${DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
            .format(movieShowtime.showtime.startAt)} " +
          s"(Cinema ${movieShowtime.cinema.id})"
    
    def selectMovieShowtime(
        movieShowtimes: List[MovieShowtime], 
        selector: String
    ): MovieShowtime = movieShowtimes.apply(selector.toInt - 1)

}

object MovieShowtimeView {
    def printMovieShowtimes(movieShowtimes: List[MovieShowtime]): Unit = {
        movieShowtimes.zipWithIndex.map { case (movieShowtime, index) => {
            val order = index + 1
            println(s"$order. ${MovieShowtimePresenter.formatMenuItem(movieShowtime)}")
        }}
    }

}

case class MovieTicket(movieShowtime: MovieShowtime)

case class MovieTicketPresenter(
    movie: String,
    movieLanguage: String,
    cinema: Int,
    showtime: LocalDateTime 
)

object MovieTicketPresenter {
    def apply(movieTicket: MovieTicket): MovieTicketPresenter = {
        val movieTitle = movieTicket.movieShowtime.movie.title
        val movieLanguage = movieTicket.movieShowtime.language.id
        val cinemaId = movieTicket.movieShowtime.cinema.id
        val showtime = movieTicket.movieShowtime.showtime.startAt

        MovieTicketPresenter(movieTitle, movieLanguage, cinemaId, showtime)
    }

}

trait MovieTicketView {
    def promptForMovieShowtimeSelection(movieShowtimes: List[MovieShowtime]): MovieShowtime
    def displayMovieTicket(movieTicketPresenter: MovieTicketPresenter): Unit

}

object ConsoleMovieTicketView extends MovieTicketView {
    def displayMovieTicket(movieTicket: MovieTicketPresenter): Unit = {
        val movieShowtimeDateTime = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm").format(movieTicket.showtime)

        println(s"You have buy a ticket.")
        println(s"+-----------------------------------+")
        println(s"|                                   |")
        println(s"|  ${movieTicket.movie} ${movieTicket.movieLanguage} ")
        println(s"|                                   |")
        println(s"|   Cinema ${movieTicket.cinema}                        |")
        println(s"|   at ${movieShowtimeDateTime}             |")
        println(s"|                                   |")
        println(s"+-----------------------------------+")
    }

    def promptForMovieShowtimeSelection(movieShowtimes: List[MovieShowtime]): MovieShowtime = {
        println("------------------------ Movie Showtimes -----------------------------")
        MovieShowtimeView.printMovieShowtimes(movieShowtimes)
        
        val input = Main.promptForUserInput
        MovieShowtimePresenter.selectMovieShowtime(movieShowtimes, input)
    }

}

// NOTE: ชื่อ Object ไม่สะท้อนกับสิ่งที่ App ทำได้จริง
// ใน Scala เราจะไม่ไ่ช้ชื่อไฟล์ว่า main.scala แต่จะใช้ชื่อไฟล์ ให้ตรงกันกับชื่อ Object หรือ Class
// TODO: เปลี่ยนชื่อไฟล์เป็น MovieTicketBooking.scala (ใช้ Pascal case)
object Main {
    def main(args: Array[String]): Unit = {
        val movieShowtimes: List[MovieShowtime] = MovieShowtime.generateMovieShowListMock // TODO: แก้ชื่อตัวแปรให้ตรงกับ Model
        val selectedMovieShowtime = ConsoleMovieTicketView.promptForMovieShowtimeSelection(movieShowtimes)
        val movieTicket = MovieTicket(selectedMovieShowtime)
        val movieTicketPresenter = MovieTicketPresenter(movieTicket)

        ConsoleMovieTicketView.displayMovieTicket(movieTicketPresenter)
    }

    def isOnlyDigits(string: String): Boolean = string.forall(_.isDigit)

    def isEscapeKey(string: String): Boolean = string.charAt(0) == 27

    def promptForUserInput(): String = new BufferedReader(new InputStreamReader(System.in)).readLine()

    def validateAcceptString(acceptStrings: Set[String], string: String): String =
        string match {
            case _ if isEscapeKey(string) => "EXIT"
            case _ if acceptStrings.contains(string) => string
            case _ => "PROMPT"
        }

}
