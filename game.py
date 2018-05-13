import pygame
import math
import numpy as np


def collide(ball1, ball2, *args):
    dX = ball1.X - ball2.X
    distance = np.sqrt(np.sum(dX**2))
    if distance < ball1.size + ball2.size:
        offset = abs(distance - (ball1.size + ball2.size))
        ball1.X -= ball1.V*dT*offset/distance

        #offset = distance - (ball1.size + ball2.size)
        #ball1.X += (-dX/distance)*offset/2
        #ball2.X += (dX/distance)*offset/2
        dV1 = -(np.inner(ball1.V-ball2.V,ball1.X-ball2.X)/np.sum((ball1.X-ball2.X)**2))*(ball1.X-ball2.X)
        dV2 = -(np.inner(ball2.V-ball1.V,ball2.X-ball1.X)/np.sum((ball2.X-ball1.X)**2))*(ball2.X-ball1.X)
        ball1.V += dV1
        ball2.V += dV2


class Line:
    def __init__(self, x1, y1, x2, y2, width=10, *args):
        self.x1 = SCALING*x1
        self.y1 = SCALING*y1
        self.x2 = SCALING*x2
        self.y2 = SCALING*y2
        self.color = (0, 0, 0)
        self.width = width

    def display(self):
        pygame.draw.line(screen, self.color, (self.x1, self.y1), (self.x2, self.y2), self.width)


class Ball:
    def __init__(self, x, y, color, dx=0, dy=0, *args):
        self.X = np.asarray([np.float64(SCALING*x), np.float64(SCALING*y)])
        self.size = SCALING*1
        self.thickness = 0
        self.color = color
        self.V = np.asarray([np.float64(SCALING*dx), np.float64(-SCALING*dy)])
        self.elasticity = SCALING*dT*2e-06
        #self.elasticity = 0
        self.f = SCALING*dT*1.614e-10
        #self.f = 0
        self.pos = []
        self.pos.append(self.X[0])
        self.pos.append(self.X[1])


    # Draw onto Screen
    def display(self):
        pygame.draw.circle(screen,
            self.color,
            (int(self.X[0].item()), int(self.X[1].item())),  # Pygame Only Draws Integer Coordinates
            self.size,
            self.thickness)

    def move(self, *args):
        self.X += self.V*dT

    def bounce(self):
        if self.X[0] > width - self.size:
            if not self.inside():
                self.X[0] = 2*(width - self.size) - self.X[0]
                self.V[0] *= -1
                speed = np.sqrt(np.sum(self.V)**2)*dT
                speed_prime = speed - self.elasticity
                if speed_prime >=0:
                    product = speed_prime / speed
                    self.V[0] *= product
                    self.V[1] *= product
                else:
                    self.V[0] *= 0
                    self.V[1] *= 0
                return False
            else:
                return True

        elif self.X[0] < self.size:
            if not self.inside():
                self.X[0] = 2*self.size - self.X[0]
                self.V[0] *= -1
                speed = np.sqrt(np.sum(self.V)**2)*dT
                speed_prime = speed - self.elasticity
                if speed_prime >=0:
                    product = speed_prime / speed
                    self.V[0] *= product
                    self.V[1] *= product
                else:
                    self.V[0] *= 0
                    self.V[1] *= 0
                return False
            else:
                return True

        if self.X[1] > height - self.size:
            if not self.inside():
                self.X[1] = 2*(height - self.size) - self.X[1]
                self.V[1] *= -1
                speed = np.sqrt(np.sum(self.V)**2)*dT
                speed_prime = speed - self.elasticity
                if speed_prime >=0:
                    product = speed_prime / speed
                    self.V[0] *= product
                    self.V[1] *= product
                else:
                    self.V[0] *= 0
                    self.V[1] *= 0
                return False
            else:
                return True

        elif self.X[1] < self.size:
            if not self.inside():
                self.X[1] = 2*self.size - self.X[1]
                self.V[1] *= -1
                speed = np.sqrt(np.sum(self.V)**2)*dT
                speed_prime = speed - self.elasticity
                if speed_prime >=0:
                    product = speed_prime / speed
                    self.V[0] *= product
                    self.V[1] *= product
                else:
                    self.V[0] *= 0
                    self.V[1] *= 0
                return False
            else:
                return True


    def friction(self, *args):
        self.pos.append(self.X[0])
        self.pos.append(self.X[1])
        if math.hypot((self.pos[2] - self.pos[0]), (self.pos[3] - self.pos[1])) >= SCALING*1:
            speed = np.sqrt(np.sum(self.V)**2)*dT
            speed_prime = speed - self.f
            if speed_prime >=0:
                product = speed_prime / speed
                self.V[0] *= product
                self.V[1] *= product
            else:
                self.V[0] *= 0
                self.V[1] *= 0
            for _ in range(4):
                self.pos.pop()
            self.pos.append(self.X[0])
            self.pos.append(self.X[1])
        else:
            for _ in range(2):
                self.pos.pop()

    def inside(self):
        if self.X[0] <= SCALING*0 + SCALING*self.size or self.X[0] >= SCALING*100 - SCALING*self.size:
            if self.X[1] <= SCALING*5 or self.X[1] >= SCALING*45:
                return True
        elif SCALING*5 >= self.X[0]  or SCALING*52.5  >= self.X[0] >= SCALING*47.5  or self.X[0] >= SCALING*95:
            if self.X[1] <= SCALING*0 + SCALING*self.size or self.X[1] >= SCALING*50 - SCALING*self.size:
                return True


SCALING = 10
dT = 1000
# Setting Background Color to Green
background_colour = (2, 178, 106)
# Width, Height, Caption
(width, height) = (SCALING*100, SCALING*50)
screen = pygame.display.set_mode((width, height))
pygame.display.set_caption('Pool Game')
clock = pygame.time.Clock()

cue_ball = Ball(80, 7, (255, 255, 255), -0.0000432, -0.0000773)
ball_one = Ball(35, 7, (255, 0, 0))
ball_two = Ball(33, 32, (255, 0, 255))
ball_three = Ball(63, 37, (0, 0,255))
ball_four = Ball(77, 28, (255,255,0))

balls = []
balls.append(cue_ball)
balls.append(ball_one)
balls.append(ball_two)
balls.append(ball_three)
balls.append(ball_four)

line1 = Line(0, 0, 0, 5)
line2 = Line(0, 0, 5, 0)
line3 = Line(0, 50, 0, 45)
line4 = Line(0, 50, 5, 50)
line5 = Line(100, 50, 100, 45)
line6 = Line(100, 50, 95, 50)
line7 = Line(100, 0, 100, 5)
line8 = Line(100, 0, 95, 0)
line9 = Line(47.5, 50, 52.5, 50)
line10 = Line(47.5, 0, 52.5, 0)

lines = []
lines.append(line1)
lines.append(line2)
lines.append(line3)
lines.append(line4)
lines.append(line5)
lines.append(line6)
lines.append(line7)
lines.append(line8)
lines.append(line9)
lines.append(line10)

running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    screen.fill(background_colour)
    for line in lines:
        line.display()
    for i, ball in enumerate(balls):
        ball.friction()
        ball.move()
        ball.bounce()
        if ball.bounce() == True:
            balls.remove(ball)
        for ball2 in balls[i+1:]:
            collide(ball, ball2)
        ball.display()

    pygame.display.flip()
    clock.tick(1000)
