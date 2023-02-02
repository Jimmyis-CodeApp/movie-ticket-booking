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

case class Show(
    movie: Movie,
    language: MovieLanguage,
    cinema: Cinema,
    showtime: Showtime
)

case class MovieTicket(
    show: Show
)

object MovieTicket {
    def buy(show: Show) = MovieTicket(show = show)
}

case class MovieTicketPresenter(
    movie: String, 
    cinema: Int, 
    showtime: LocalDateTime 
)

object MovieTicketPresenter {
    def createPrintTicket(movieTicket: MovieTicket): MovieTicketPresenter = {
        val movieTitle = movieTicket.show.movie.title
        val cinemaId = movieTicket.show.cinema.id
        val showtime = movieTicket.show.showtime.startAt
        new MovieTicketPresenter(movieTitle, cinemaId, showtime)
    }

    def printFormattedTicket(movieTicket: MovieTicketPresenter) = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
        println(s"Buying a ticket for ${movieTicket.movie} at ${dateTimeFormat.format(movieTicket.showtime)} (Cinema ${movieTicket.cinema})")
    }
}

object MovieTicketBooking {
    def main(args: Array[String]): Unit = {
        val movieName = "Avatar 2 [TH]"
        val cinemaNo = 1
        val movieTime = LocalDateTime.parse("2023-01-25T18:15:00")
        val ticket = MovieTicketPresenter.buy(movieName, cinemaNo, movieTime)
        
        MovieTicketPresenter.printFormattedTicket(ticket)
    }

}

