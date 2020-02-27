library(R6)

Cards <- R6Class(
  "Cards",
  public = list(
    cards = list(),
    size = 0,
    initialize = function() {
      farbe <- c("G", "H", "E", "S")
      wert <- c(6:10, "U", "O", "K", "A")
      cards <- paste0(rep(farbe, each = 9), rep(wert, times = 4))
      self$cards <- sample(cards, 36)
      self$size <- 36
    },
    draw = function(num_cards) {
      if (typeof(num_cards) != "integer") stop("Please draw an integer amount of cards!")
      if (num_cards > length(self$cards)) stop("You can't draw more cards than on the stack!")
      drawn_indices <- sample(self$size, num_cards)
      drawn_cards <- self$cards[drawn_indices]
      self$cards <- self$cards[-drawn_indices]
      self$size <- self$size - num_cards
      drawn_cards
    },
    reshuffle = function() {
      self$initialize()
    },
    cut = function(num_cards) {
      if (typeof(num_cards) != "integer") stop("Please cut an integer amount of cards!")
      if (num_cards > length(self$cards)) stop("You can't cut more cards than on the stack!")
      cut_indices <- tail(seq_len(self$size), num_cards)
      self$cards <- c(self$cards[cut_indices], self$cards[-cut_indices]) 
    }
  )
)

