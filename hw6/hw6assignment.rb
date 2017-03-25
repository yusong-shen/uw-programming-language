# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # 3 new enhencement pieces with 7 classic pieces
  All_My_Pieces = [
               # [[0, 0], [1, 0], [0, 1], [1, 1]],  # square (only needs one)
               # rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               # [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               # [[0, 0], [0, -1], [0, 1], [0, 2]]],
               # rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               # rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               # rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               # rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]]), # 2 + 3
               rotations([[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]]), # long with 5
               # rotations([[0, 0], [1, 0], [0, 1]]) # 1 + 2
               ]

  # your enhancements here
  # override to use All_My_Pieces
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board
  # your enhancements here

  # override to use MyPiece
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # rotates the current piece 180 degree
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # override to use MyPiece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end  

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..4).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end 
end

class MyTetris < Tetris
  # your enhancements here
  # override to use MyBoard
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
      super
      @root.bind('u', proc {@board.rotate_180})
  end
end


