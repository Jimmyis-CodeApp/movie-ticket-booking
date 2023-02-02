import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class MovieTicketPresenter( 
    movie: String, 
    cinema: Int, 
    time: LocalDateTime 
)

object MovieTicketPresenter {
    def buy(
        movie: String,
        cinema: Int,
        time: LocalDateTime
    ): MovieTicketPresenter = new MovieTicketPresenter(movie, cinema, time)

    def printFormattedTicket(movieTicket: MovieTicketPresenter) = {
        val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
        println(s"Buying a ticket for ${movieTicket.movie} at ${dateTimeFormat.format(movieTicket.time)} (Cinema ${movieTicket.cinema}")
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

