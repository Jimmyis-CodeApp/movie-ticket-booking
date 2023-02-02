import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

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
    def displayShowMenuItem(show: MovieShow): String = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
        s"${show.movie.title} (${show.language.title}) show start at ${dateTimeFormat.format(show.showtime.startAt)} (Cinema ${show.cinema.id})"
    }
}

case class MovieTicket(
    show: MovieShow
)

object MovieTicket {
    def buy(show: MovieShow) = MovieTicket(show = show)
}

case class MovieTicketPresenter(
    movie: String,
    movieLanguage: String,
    cinema: Int,
    showtime: LocalDateTime 
)

object MovieTicketPresenter {
    def createPrintTicket(movieTicket: MovieTicket): MovieTicketPresenter = {
        val movieTitle = movieTicket.show.movie.title
        val movieLanguage = movieTicket.show.language.id
        val cinemaId = movieTicket.show.cinema.id
        val showtime = movieTicket.show.showtime.startAt
        new MovieTicketPresenter(movieTitle, movieLanguage, cinemaId, showtime)
    }

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
        val shows = List[MovieShow](
            MovieShow(
                movie = Movie(title = "Avatar 2"),
                language = MovieLanguage(id = "TH", title = "Thai"),
                cinema = Cinema(id = 1),
                showtime = Showtime(
                    startAt = LocalDateTime.parse("2023-01-25T18:15:00"),
                    endAt = LocalDateTime.parse("2023-01-25T18:15:00")
                )
            ),
            MovieShow(
                movie = Movie(title = "Avatar 2"),
                language = MovieLanguage(id = "EN", title = "Soundtrack (English)"),
                cinema = Cinema(id = 2),
                showtime = Showtime(
                    startAt = LocalDateTime.parse("2023-01-25T18:15:00"),
                    endAt = LocalDateTime.parse("2023-01-25T18:15:00")
                )
            ),
            MovieShow(
                movie = Movie(title = "M3GAN"),
                language = MovieLanguage(id = "EN", title = "Soundtrack (English)"),
                cinema = Cinema(id = 3),
                showtime = Showtime(
                    startAt = LocalDateTime.parse("2023-01-25T18:15:00"),
                    endAt = LocalDateTime.parse("2023-01-25T18:15:00")
                )
            )
        )
        runningLoop(shows)
    }

    def runningLoop(shows: List[MovieShow]): Unit = {
        val selectedShow = promptMenu[MovieShow](shows, "Please select show you want to buy a ticket", MovieShow.displayShowMenuItem)
        val movieTicket = MovieTicket.buy(selectedShow)
        val printedMovieTicket = MovieTicketPresenter.createPrintTicket(movieTicket)
        MovieTicketPresenter.printFormattedTicket(printedMovieTicket)
        println("\n")
        runningLoop(shows)
    }

    def promptMenu[T](items: List[T], message: String, displayFormatter: T => String): T = {
        println("------------------------ Movie Showtimes -----------------------------")
        val options = items.zipWithIndex
        options.map{ case (option, index) => println(s"${index + 1}. " + displayFormatter(option)) }
        val input = scala.io.StdIn.readLine()
        val selectedOption = if (input.length > 0 && onlyDigits(input)) input.toInt else -1
        
        if (selectedOption >= 1 && selectedOption < (items.length + 1)) {
            val selectedItem = items.apply(selectedOption.toInt - 1)
            println("-----------------------------------------------------------")
            println("| You selected " + displayFormatter(selectedItem))
            println("-----------------------------------------------------------")
            return selectedItem
        }
        else {
            println("Please select a movie show you want to buy a ticket")
            return promptMenu(items, message, displayFormatter)
        }
    }

    def onlyDigits(s: String): Boolean = s.forall(_.isDigit)
}
