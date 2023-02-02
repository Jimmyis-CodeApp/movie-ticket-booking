import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class MovieTicket( 
    movie: String, 
    cinema: Int, 
    time: LocalDateTime 
)

object MovieTicket {
    def buy(
        movie: String,
        cinema: Int,
        time: LocalDateTime
    ): MovieTicket = new MovieTicket(movie, cinema, time)
}

object MovieTicketBooking {
    def main(args: Array[String]): Unit = {
        val movieName = "Avatar 2 [TH]"
        val cinemaNo = 1
        val movieTime = LocalDateTime.parse("2023-01-25T18:15:00")
        val ticket = MovieTicket.buy(movieName, cinemaNo, movieTime)

        val dtformat = DateTimeFormatter.ofPattern("dd/MM/YYYY:HH.mm")
        
        println(s"Buying a ticket for ${ticket.movie} at ${dtformat.format(ticket.time)} (Cinema ${ticket.cinema}")

    }

}

