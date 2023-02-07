import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// TODO: Follow Code Formatting and Convention
// TODO: Return type to all function

case class Movie(
    title: String
)

case class MovieLanguage(
    id: String,
    title: String
)

case class Cinema(
    id: Int
)

case class Showtime(
    startAt: LocalDateTime,
    endAt: LocalDateTime
)

case class MovieShow(
    movie: Movie,
    language: MovieLanguage,
    cinema: Cinema,
    showtime: Showtime
)

object MovieShow {
    def generateMovieShowListMock: List[MovieShow] = {
        List[MovieShow](
            // NOTE:  เรียกว่าอะไร?
            MovieShow(
                movie = Movie("Avatar 2"),
                language = MovieLanguage("TH", "Thai"),
                cinema = Cinema(1),
                showtime = Showtime(
                    startAt = LocalDateTime.parse("2023-01-25T18:15:00"),
                    endAt = LocalDateTime.parse("2023-01-25T18:15:00")
                )
            ),
            MovieShow(
                movie = Movie("Avatar 2"),
                language = MovieLanguage("EN", "Soundtrack (English)"),
                cinema = Cinema(2),
                showtime = Showtime(
                    startAt = LocalDateTime.parse("2023-01-25T18:15:00"),
                    endAt = LocalDateTime.parse("2023-01-25T18:15:00")
                )
            ),
            MovieShow(
                movie = Movie("M3GAN"),
                language = MovieLanguage("EN", "Soundtrack (English)"),
                cinema = Cinema(3),
                showtime = Showtime(
                    startAt = LocalDateTime.parse("2023-01-25T18:15:00"),
                    endAt = LocalDateTime.parse("2023-01-25T18:15:00")
                )
            )
        )
    }

}

object MovieShowPresenter {
    def formatMenuItem(show: MovieShow): String = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
        s"${show.movie.title} (${show.language.title}) show start at ${dateTimeFormat.format(show.showtime.startAt)} (Cinema ${show.cinema.id})"

    }
    
}

case class MovieTicket(
    show: MovieShow
)

object MovieTicket {
    def buy(show: MovieShow) = MovieTicket(show)
}

case class MovieTicketPresenter(
    movie: String,
    movieLanguage: String,
    cinema: Int,
    showtime: LocalDateTime 
)

object MovieTicketPresenter {
    def apply(movieTicket: MovieTicket): MovieTicketPresenter = {
        val movieTitle = movieTicket.show.movie.title
        val movieLanguage = movieTicket.show.language.id
        val cinemaId = movieTicket.show.cinema.id
        val showtime = movieTicket.show.showtime.startAt

        new MovieTicketPresenter(movieTitle, movieLanguage, cinemaId, showtime)
    }

}

object MovieTicketView {
    def printFormattedTicket(movieTicket: MovieTicketPresenter) = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
        println(s"You have buy a ticket.")
        println(s"+-----------------------------------+")
        println(s"|                                   |")
        println(s"|  ${movieTicket.movie} ${movieTicket.movieLanguage} ")
        println(s"|                                   |")
        println(s"|   Cinema ${movieTicket.cinema}                        |")
        println(s"|   at ${dateTimeFormat.format(movieTicket.showtime)}             |")
        println(s"|                                   |")
        println(s"+-----------------------------------+")
    }

}

object MovieTicketBooking {
    def main(args: Array[String]): Unit = {
        val shows = MovieShow.generateMovieShowListMock
        runningLoop(shows)
    }

    def runningLoop(shows: List[MovieShow]): Unit = {
        val selectedShow = promptMenu[MovieShow](shows, "Please select show you want to buy a ticket", MovieShowPresenter.formatMenuItem)
        val movieTicket = MovieTicket.buy(selectedShow)
        val printedMovieTicket = MovieTicketPresenter(movieTicket)
        MovieTicketView.printFormattedTicket(printedMovieTicket)
        println("\n")
        runningLoop(shows)
    }

    // NOTE: ทำเป็น Generic ทำไม? ทำเป็น Recursion ทำไม?
    // NOTE: Generic เป็นของที่ Design ยากที่สุด จาก ปสก พี่เดฟ... อย่าทำ ถ้าไม่มี Specific Case ที่มัน Work แน่ๆ
    // อย่าเริ่มจาก Generic ก่อน แล้วเข้ามาหาความ Specific
    def promptMenu[T](items: List[T], message: String, displayFormatter: T => String): T = {
        println("------------------------ Movie Showtimes -----------------------------") // NOTE: ใส่ทำไม
        val options = items.zipWithIndex
        options.map { case (option, index) => println(s"${index + 1}. " + displayFormatter(option)) }
        val input = scala.io.StdIn.readLine()
        val selectedOption = if (input.length > 0 && isOnlyDigits(input)) input.toInt else -1
        
        if (selectedOption >= 1 && selectedOption < (items.length + 1)) { // TODO: แก้ เลข 1 (อย่าให้เป็น Magic Number)
            val selectedItem = items.apply(selectedOption.toInt - 1)
            println("-----------------------------------------------------------")
            println("|  You selected " + displayFormatter(selectedItem))
            println("-----------------------------------------------------------")
            return selectedItem
        } else {
            println("Please select a movie show you want to buy a ticket") // NOTE: เหมือน Line 137 ใส่มาทำไม?
            return promptMenu(items, message, displayFormatter)
        }
    }

    def isOnlyDigits(s: String): Boolean = s.forall(_.isDigit)
}
