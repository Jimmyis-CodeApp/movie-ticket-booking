import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.util.control.Breaks._

case class Movie(title: String)

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
    def generateMovieShowListMock: List[MovieShowtime] = {
        List[MovieShowtime](
            // NOTE:  เรียกว่าอะไร?
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

}

object MovieShowtimePresenter {
    def formatMenuItem(show: MovieShowtime): String = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")

        s"${show.movie.title} (${show.language.title}) show start at ${dateTimeFormat.format(show.showtime.startAt)} (Cinema ${show.cinema.id})"
    }
    
}

object MovieShowtimeView {
    def printMovieShowSelectionList(movieShowSelectionList: List[(Int, String)]): Unit = {
        for (case (index, item) <- movieShowSelectionList) {
            println(s"${index}. ${item}")
        }
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

        new MovieTicketPresenter(movieTitle, movieLanguage, cinemaId, showtime)
    }

    def generateMovieShowSelectionList(movieList: List[MovieShowtime]): List[(Int, String)] = {
        val options = movieList.zipWithIndex
        options.map { case (option, index) => (index + 1, MovieShowtimePresenter.formatMenuItem(option)) }
    }

    def generateMovieShowSelectableSet(movieShowSelectionList: List[(Int, String)]): Set[String] = {
        movieShowSelectionList.map { case (index, item) => index.toString }.toSet
    }

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
        println(s"|   at ${dateTimeFormat.format(movieTicket.showtime)}             |")
        println(s"|                                   |")
        println(s"+-----------------------------------+")
    }

}

object MovieTicketBooking {
    def main(args: Array[String]): Unit = {
        val movieShows = MovieShowtime.generateMovieShowListMock
        val movieShowSelectionList = MovieTicketPresenter.generateMovieShowSelectionList(movieShows)

        println("------------------------ Movie Showtimes -----------------------------")
        MovieShowtimeView.printMovieShowSelectionList(movieShowSelectionList)

        val movieShowSelectionSet = MovieTicketPresenter.generateMovieShowSelectableSet(movieShowSelectionList)
        val input = processInput(movieShowSelectionSet)

        if (input == "EXIT") {
            println("App is quiting, Bye bye")
            sys.exit(1)
        }
        
        val selectedMovieShow = movieShows.apply(input.toInt)
        val movieTicket = MovieTicket(selectedMovieShow)
        val printedMovieTicket = MovieTicketPresenter(movieTicket)
        MovieTicketView.printFormattedTicket(printedMovieTicket)
    }

    def isOnlyDigits(s: String): Boolean = s.forall(_.isDigit)

    def processInput(acceptInputsList: Set[String]): String = {
        val br = new BufferedReader(new InputStreamReader(System.in))
        var input = br.readLine()

        breakable {
            while (input != null && input.length != 0) {
                // Check if the first character is the ASCII value for Esc (27)
                if (input.charAt(0) == 27) { 
                    input = "EXIT"
                    break()
                } else if (acceptInputsList.contains(input)) {
                    break()
                } else {
                    input = br.readLine()
                }
            }
        }

        return input
    }

}
