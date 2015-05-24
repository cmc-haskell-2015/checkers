#!/usr/bin/env python
#-*- coding:utf8 -*-

import re
from collections import namedtuple
import sys

if sys.version_info[0] == 3:
    xrange = range
    raw_input = input

################################################################################
################################################################################ Core

Color = int
BLACK = 0
WHITE = 1

def nextcolor(c):
    return 1 - c

GuyType = int
SLAVE = 0
KING = 1

class Pos (object):
    """ Координата на поле. хешируема и сравниваема """
    def __init__(self, row, col):
        self.row = row
        self.col = col
    def __hash__(self):
        return hash(self.row) + hash(self.col)*12793
    def __str__(self):
        return 'Pos({}, {})'.format(self.row, self.col)
    def __eq__(self, other):
        return self.row == other.row and self.col == other.col
    def __ne__(self, other):
        return not self == other
    def __add__(self, other):
        if type(other) == Pos:
            return Pos(self.row + other.row, self.col + other.col)
        elif type(other) == tuple:
            return Pos(self.row + other[0], self.col + other[1])

class _Guy (object):
    """ Одна шашка.

    Используется только внутри Core, так что не суть.
    Вообще, возможно здесь же стоило сделать и координату (а может, и не стоило)
    """
    def __init__(self, color, type=SLAVE):
        self.type = type
        self.color = color

""" Тип перемещения
Что важно, тип _одного_ перемещения.
Соответственно, один чувак имеет несколько movement'ов

fr, to: Pos -- откуда и куда переместились
eatenGuys: list(..) -- кого съели
becameKing: bool -- не стали ли дамкой
final: bool -- можно ли двигаться дальше
first: bool -- первый ли ход (нужно для unexecute)
"""
Movement = namedtuple('Movement', [ 'fr', 'to', 'eatenGuys', 'becameKing', 'final', 'first' ])

class GameCore (object):
    """ Ядро игры

    Отвечает за хранение состояния игры +
        определение, кто ходит, может ли сходить etc.
    """
    BOARD_SIZE = 8 # размер доски
    GUYS_ROWS_COUNT = 3 # количество строк с чуваками

    def __init__(self):
        self.guys = { BLACK: set(), WHITE: set() } # coords only
        self.field = [ [ None for col in xrange(self.BOARD_SIZE) ]
                       for row in xrange(self.BOARD_SIZE) ]

        # init the field
        for row in xrange(self.GUYS_ROWS_COUNT):
            for col in xrange(self.BOARD_SIZE):
                if (row + col)%2 == 0:
                    self.guys[WHITE].add(Pos(row, col))
                    self.field[row][col] = _Guy(WHITE)

        for row in xrange(self.BOARD_SIZE - self.GUYS_ROWS_COUNT, self.BOARD_SIZE):
            for col in xrange(self.BOARD_SIZE):
                if (row + col)%2 == 0:
                    self.guys[BLACK].add(Pos(row, col))
                    self.field[row][col] = _Guy(BLACK)

    @property
    def winner(self):
        """ Получить победителя игры, если таковой имеется """
        if min(len(self.guys[BLACK]), len(self.guys[WHITE])) > 0:
            return None
        elif self.guys[BLACK] == 0:
            return WHITE
        else:
            return BLACK

    def get_guy(self, pos):
        return self.field[pos.row][pos.col]

    def get_moves_by_pos(self, pos, is_first):
        """ Получить список всех перемещений из данной координаты """
        guy = self.get_guy(pos)
        if guy is None:
            return

        if guy.type == SLAVE:
            drow, lastrow = { BLACK: (-1, 0,),
                              WHITE: (+1, self.BOARD_SIZE - 1,) }[guy.color]

            if is_first and 0 <= pos.row + drow < self.BOARD_SIZE:
                npos = pos + Pos(drow, -1)
                if pos.col - 1 >= 0 and self.get_guy(npos) is None:
                    yield Movement(pos, npos, [], npos.row == lastrow, True, is_first)

                npos = pos + Pos(drow, +1)
                if pos.col + 1 < self.BOARD_SIZE and self.get_guy(npos) is None:
                    yield Movement(pos, npos, [], npos.row == lastrow, True, is_first)

            if 0 <= pos.row - 2 < self.BOARD_SIZE:
                tpos = pos + Pos(-1, -1)
                npos = pos + Pos(-2, -2)
                if pos.col - 2 >= 0 and not self.get_guy(tpos) is None \
                                    and self.get_guy(npos) is None:
                    eatenGuys = [ tpos ] if self.get_guy(tpos).color != guy.color else []
                    yield Movement(pos, npos, eatenGuys, npos.row == lastrow, False, is_first)

                tpos = pos + Pos(-1, +1)
                npos = pos + Pos(-2, +2)
                if pos.col + 2 < self.BOARD_SIZE and not self.get_guy(tpos) is None \
                                                 and self.get_guy(npos) is None:
                    eatenGuys = [ tpos ] if self.get_guy(tpos).color != guy.color else []
                    yield Movement(pos, npos, eatenGuys, npos.row == lastrow, False, is_first)

            if 0 <= pos.row + 2 < self.BOARD_SIZE:
                tpos = pos + Pos(1, -1)
                npos = pos + Pos(2, -2)
                if pos.col - 2 >= 0 and not self.get_guy(tpos) is None \
                                    and self.get_guy(npos) is None:
                    eatenGuys = [ tpos ] if self.get_guy(tpos).color != guy.color else []
                    yield Movement(pos, npos, eatenGuys, npos.row == lastrow, False, is_first)

                tpos = pos + Pos(1, +1)
                npos = pos + Pos(2, +2)
                if pos.col + 2 < self.BOARD_SIZE and not self.get_guy(tpos) is None \
                                                 and self.get_guy(npos) is None:
                    eatenGuys = [ tpos ] if self.get_guy(tpos).color != guy.color else []
                    yield Movement(pos, npos, eatenGuys, npos.row == lastrow, False, is_first)
        else:
            raise NotImplementedError()


    def get_moves_by_color(self, color):
        """ Получить список всех перемещений

        Конечно же, параметра is_first нет
        """
        for pos in self.guys[color]:
            for x in self.get_moves_by_pos(pos, True):
                yield x
        return

    def find_move(self, fr, to, is_first):
        """ найти такое перемещение, если оно есть """
        for move in self.get_moves_by_pos(fr, is_first):
            if move.to == to:
                return move
        return None

    def valid_move(self, fr, to, is_first):
        """ Проверить валидность перемещения """
        return not self.find_move(fr, to, is_first) is None

    def exec_movement(self, move):
        """ Запустить movement """
        guy = self.get_guy(move.fr)
        if move.becameKing:
            guy.type = KING

        for eaten in move.eatenGuys:
            self.guys[self.get_guy(eaten).color].remove(eaten)
            self.field[eaten.row][eaten.col] = None

        self.field[move.fr.row][move.fr.col] = None
        self.field[move.to.row][move.to.col] = guy

        self.guys[guy.color].remove(move.fr)
        self.guys[guy.color].add(move.to)

    def unexec_movement(self, move):
        """ Отменить данный movement

        В этой реализации этого сделать нельзя (ну не подумал сразу),
            но в haskell-реализации в Movement'е содержится больше инфы, =>
            его одного достаточно для того, чтобы откатить любой ход
        """
        raise NotImplementedError()

    def make_move(self, fr, to, is_first):
        """ Обёртка над exec_movement.

        Вообще говоря, кажется, она не используется
        """
        move = self.find_move(fr, to, is_first)
        assert not move is None

        self.exec_movement(move)

################################################################################
################################################################################ Player

class PlayerBase (object):
    """ Класс игрока

    Потомками класса могут быть как модуль ИИ, так и gui-интерфейс
    """
    def __init__(self, color):
        self.color = color

    def wait_for_movement(self, game, fr=None):
        """ Получить от пользователя ход

        Предполагается, что если задано fr, то ходить можно только
            парнем в этой точке

        Возвращает список ходов (например, если я в консоли ввёл c3-b5-c7),
            то должно вернуться [(c3 -> b5), (b5 -> c7)].
            Но за правильность и адекватность этих ходов он не отвечает
        """
        raise NotImplementedError()

    def invite(self):
        """ Сказать пользователю, что можно ходить """
        raise NotImplementedError()

    def bad_movement(self):
        """ Сказать пользователю, что ход был хуёвым.

        Скорее всего, надо бы ещё аргументов добавить. хз пока
        """
        raise NotImplementedError()

class PlayerConsole (PlayerBase):
    """ Реализация для игры через консоль """
    MOVES_RE = re.compile(r'moves\s+(?P<pos>\w{2})?')

    def __init__(self, color):
        super(PlayerConsole, self).__init__(color)

    @classmethod
    def __str2pos(cls, s):
        return Pos(int(s[1]) - 1, ord(s[0]) - ord('a'))

    @classmethod
    def __pos2str(cls, pos):
        return chr(pos.col + ord('a')) + str(pos.row + 1)

    def __print_moves(self, game, pos, is_first):
        for move in game.get_moves_by_pos(pos, is_first):
            print ('{} -> {}'.format(self.__pos2str(move.fr), self.__pos2str(move.to)))

    def wait_for_movement(self, game, fr=None):
        if not fr is None:
            print ("Continue moving guy at {}".format(self.__pos2str(fr)))

        while True:
            s = raw_input().strip()
            m = self.MOVES_RE.match(s)
            if m:
                if not m.group('pos') is None:
                    pos = self.__str2pos(m.group('pos'))
                elif not fr is None:
                    pos = fr
                else:
                    print ("Don't know which cell should be analyzed")
                self.__print_moves(game, pos, fr is None)
            else:
                s = list(map(str.strip, s.split('-')))
                if len(s) < 2:
                    print ("Too small input")
                    continue

                try:
                    path = list(map(self.__str2pos, s))
                except:
                    print ("Bad input, try again")
                    continue

                if not fr is None and path[0] != fr:
                    print ("Should move the given guy!")
                    continue

                return  [ (path[i - 1], path[i],) for i in xrange(1, len(path)) ]

    def invite(self):
        """ Сказать пользователю, что можно ходить """
        print ("Player {}, your turn!".format(2 - self.color))

    def bad_movement(self):
        """ Сказать пользователю, что ход был хуёвым.

        Скорее всего, надо бы ещё аргументов добавить. хз пока
        """
        print ("Bad movement, try again")

################################################################################
################################################################################ Graphics

class GraphBase (object):
    """ Класс графики. Вообще эта штука должна быть сложнее """

    def repaint(self, game):
        """ Обновить всё """
        raise NotImplementedError()

class GraphConsole (GraphBase):
    """ Тёплая ламповая консольная реализация """
    GUYS_SYMBOLS = { BLACK: { SLAVE: 'x', KING: '%' },
                     WHITE: { SLAVE: 'o', KING: '@' }}
    @staticmethod
    def __get_border(width):
        return '+' + '---+'*width

    def repaint(self, game):
        for row in xrange(game.BOARD_SIZE - 1, -1, -1):
            print ('  ' + self.__get_border(game.BOARD_SIZE))
            s = '{} |'.format(row + 1)
            for col in xrange(game.BOARD_SIZE):
                guy = game.field[row][col]
                if guy is None:
                    s += '   |'
                else:
                    s += ' {} |'.format(self.GUYS_SYMBOLS[guy.color][guy.type])
            print (s)

        print ('  ' + self.__get_border(game.BOARD_SIZE))
        s = '  '
        for col in xrange(game.BOARD_SIZE):
            s += '  {} '.format(chr(col + ord('A')))
        print (s)

################################################################################
################################################################################ Controller

class Controller (object):
    """ Контроллер игры """
    def __init__(self, playerBlack, playerWhite, graph):
        self.players = { BLACK: playerBlack, WHITE: playerWhite }
        self.graph = graph

    def _make_turn(self, game, color):
        """ Сделать ход """
        player = self.players[color]
        last_pos = None

        player.invite()
        while True:
            self.graph.repaint(game)
            if not last_pos is None and \
               len(list(game.get_moves_by_pos(last_pos, False))) == 0:
                break
            moves = player.wait_for_movement(game, last_pos)
            for fr, to in moves:
                if not game.valid_move(fr, to, last_pos is None):
                    player.bad_movement()
                    break

                move = game.find_move(fr, to, last_pos is None)
                game.exec_movement(move)

                last_pos = to

                if move.final:
                    return

    def run(self):
        """ Сыграть новую игру """
        game = GameCore()
        color = WHITE

        while game.winner is None:
            self._make_turn(game, color)
            color = nextcolor(color)
        return game.winner

################################################################################
################################################################################ Main

def __main__():
    """ Точка входа """
    c = Controller(PlayerConsole(BLACK), PlayerConsole(WHITE), GraphConsole())
    winner = c.run()
    print ('Player {} win'.forma(winner))

if __name__ == "__main__":
    __main__()
