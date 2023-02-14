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

}

object MovieShowtimePresenter {
    def formatMenuItem(movieShowtime: MovieShowtime): String = {
        // NOTE: ตัวแปรตรงนี้อาจจะไม่จำเป็น เพราะไม่มีการ Reuse
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")

        // NOTE: หา Convention ในการ ทำ String ยาว
        s"${movieShowtime.movie.title} (${movieShowtime.language.title}) show start at ${dateTimeFormat.format(movieShowtime.showtime.startAt)} (Cinema ${movieShowtime.cinema.id})"
    }
    
}

object MovieShowtimeView {
    // TODO: แก้ชื่อฟังก์ชัน + Parameter ให้สอดคล้องกับ Model
    // NOTE: Parameter Type มัน Too generalized
    // อาจจะใช้ MovieShowtimePresenter มาแทนได้
    def printMovieShowtimeSelectionList(movieShowtimeSelectionList: List[(Int, String)]): Unit =
        // NOTE: คำว่า item กำกวม
        for (case (index, item) <- movieShowtimeSelectionList) {
            println(s"${index}. ${item}") // NOTE: Convention ของการใช้ ${} ไม่จำเป็นต้องใส่ {} กับ String หรือตัวแปรโดดๆ (Primitive)
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

        // NOTE: new keyword
        new MovieTicketPresenter(movieTitle, movieLanguage, cinemaId, showtime)
    }

    // TODOS: 
    // - ย้ายไปไว้ใน MovieShowtime
    // - แก้ชื่อฟังก์ชัน ตามชื่อ Model ที่เปลี่ยนไป
    // - แก้ชื่อ Parameter movieList ให้ถูกต้องตาม type (MovieShowtime)
    // NOTE:
    // - ชื่อฟังก์ชัน ไม่สอดคล้องกับ Return type -- จะรู้ได้ไง ถ้าอ่านจากชื่อฟังก์ชั่น ว่ามัน Return MovieSHowSelectionList
    // - คำว่า option ดูกำกวมมาก
    // - ฟังก์ชั่นนี้เอาไว้ทำอะไรได้บ้าง
    def generateMovieShowtimeSelectionList(movieList: List[MovieShowtime]): List[(Int, String)] = {
        val options = movieList.zipWithIndex
        
        options.map { case (option, index) => (index + 1, MovieShowtimePresenter.formatMenuItem(option)) }
    }

    // TODOS: 
    // - ย้ายไปไว้ใน MovieShowtime
    // - แก้ชื่อฟังก์ชัน ตามชื่อ Model ที่เปลี่ยนไป
    // - Function บรรทัดเดียว ไม่ต้องใช้ { }
    // NOTES:
    // - คำว่า Selectable กำกวมมาก (ทำไมใช้คำนี้ ต่างจาก Selection ยังไง)
    // - ฟังก์ชันนี้ อาจจะไม่จำเป็น
    def generateMovieShowtimeSelectableSet(movieShowtimeSelectionList: List[(Int, String)]): Set[String] =
        movieShowtimeSelectionList.map { case (index, item) => index.toString }.toSet

}

object MovieTicketView {
    def printFormattedTicket(movieTicket: MovieTicketPresenter): Unit = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")

        println(s"You have buy a ticket.")
        println(s"+-----------------------------------+")
        println(s"|                                   |")
        println(s"|  ${movieTicket.movie} ${movieTicket.movieLanguage} ")
        println(s"|                                   |")
        println(s"|   Cinema ${movieTicket.cinema}                        |")
        println(s"|   at ${dateTimeFormat.format(movieTicket.showtime)}             |") // TODO: Process ให้เสร็จด้านที่ ให้เหลือเป็นตัวแปรตัวเดียว เช่น time
        println(s"|                                   |")
        println(s"+-----------------------------------+")
    }

}

// NOTE: ชื่อ Object ไม่สะท้อนกับสิ่งที่ App ทำได้จริง
// ใน Scala เราจะไม่ไ่ช้ชื่อไฟล์ว่า main.scala แต่จะใช้ชื่อไฟล์ ให้ตรงกันกับชื่อ Object หรือ Class
// TODO: เปลี่ยนชื่อไฟล์เป็น MovieTicketBooking.scala (ใช้ Pascal case)
object MovieTicketBooking {
    def main(args: Array[String]): Unit = {
        // NOTES: Line 154-155
        // - ทำไม ถึงไม่เอา MovieShowtimes ไปเลือกเลย (ทำไมถึงต้องไปทำ List)
        // TODO: ยุบ การ Generate MovieShowtimesSelectionList ออกไปเลย แล้วเอา MovieShowTimes ไป Print
        val movieShowtimes: List[MovieShowtime] = MovieShowtime.generateMovieShowListMock // TODO: แก้ชื่อตัวแปรให้ตรงกับ Model
        val movieShowtimeSelectionList: List[(Int, String)] = MovieTicketPresenter.generateMovieShowtimeSelectionList(movieShowtimes) // TODO: - แก้ชื่อตัวแปรให้ตรงกับ Model

        println("------------------------ Movie Showtimes -----------------------------")
        MovieShowtimeView.printMovieShowtimeSelectionList(movieShowtimeSelectionList)

        // TODO: - แก้ชื่อตัวแปรให้ตรงกับ Model
        // NOTES: 
        // คำว่า Selection/Selectable ยังดูสับสน
        // - ตอนนี้เหมือนเอา Business Logic กับ Library มาปะปนใช้งานกัน (ควรทำ Middleware) - 
        val movieShowtimeSelectionSet = MovieTicketPresenter.generateMovieShowtimeSelectableSet(movieShowtimeSelectionList) 
        // NOTES: 
        // - ชื่อ processInput ไม่สื่อถึง Business Logic -- อยู่ผิดที่ผิดทาง ไม่รู้เลยว่า ฟังก์ชันทำหน้าที่อะไร
        // - ไม่รู้ด้วยว่ามันคือ Type อะไร (ต่อให้รู้ว่ามันคือ String แต่ก็ไม่รู้ว่ามันเป็นอะไรได้บ้าง) -- Type ไม่สื่อกับการนำไปใช้
        val input = processInput(movieShowtimeSelectionSet) 
        // ** ทำไมต้อง ทำให้กดเข้าไปดูถึง Low-Level (Context ไม่มี/ไม่ชัดเจน, ชื่อไม่ตรง, Type ไม่มี)

        if (input == "EXIT") {
            println("App is quiting, Bye bye")
            sys.exit(1)
        }
        
        // TODOS: 
        // - แก้ชื่อตัวแปร
        // NOTE:
        // - ทำไม ไม่เป็นฟังก์ชันเดียว ที่ได้ SelectedMovieShowtime เลย
        val selectedMovieShowtime = movieShowtimes.apply(input.toInt) 
        val movieTicket = MovieTicket(selectedMovieShowtime)
        val printedMovieTicket = MovieTicketPresenter(movieTicket)

        MovieTicketView.printFormattedTicket(printedMovieTicket)
    }

    def isOnlyDigits(string: String): Boolean = string.forall(_.isDigit)

    def isEscapeKey(string: String): Boolean = string.charAt(0) == 27

    // TODOS: 
    // - ชื่อ parameter ไม่ชัดเจน และไม่สะท้อนกับ Type (List -> Set)
    def processInput(acceptInputsList: Set[String]): String = {
        val br = new BufferedReader(new InputStreamReader(System.in)) // TODO: เปลี่ยนชื่อตัวแปร จากตัวย่อ ให้เต็ม, NOTE: ชื่อตัวแปร อาจทำให้สับสบ
        var input = br.readLine()

        // TODOS:
        // 
        // NOTES:
        // - มันคือ Validator น่าจะเขียนให้มันง่ายกว่านี้ หรือทำเป็นฟังก์ชันแยก, น่าจะใช้ Switch/Case แทนได้
        breakable {
            while (input != null && input.length != 0) {
                // Check if the first character is the ASCII value for Esc (27)
                if (isEscapeKey(input)) {
                    input = "EXIT"
                    break()
                } else if (acceptInputsList.contains(input)) {
                    break()
                } else {
                    input = br.readLine()
                }
            }
        }

        input
    }

}
