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

    def promptMenu[T](items: List[T], message: String, displayFormatter: T => String): T = {
        println("------------------------ Movie Showtimes -----------------------------")
        val options = items.zipWithIndex
        options.map{ case (option, index) => println(s"${index + 1}. " + displayFormatter(option)) }
        val input = scala.io.StdIn.readLine()
        val selectedOption = if (input.length > 0 && onlyDigits(input)) input.toInt else -1
        
        if (selectedOption >= 1 && selectedOption < (items.length + 1)) {
            println("You selected option " + selectedOption)
            return items.apply(selectedOption.toInt - 1)
        }
        else {
            println("Please select show you want to buy a ticket")
            return promptMenu(items, message)
            return promptMenu(items, message, displayFormatter)
        }
    }

    def onlyDigits(s: String): Boolean = s.forall(_.isDigit)
}
