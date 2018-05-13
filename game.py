import pygame
import math
import numpy as np


def collide(ball1, ball2):
    dX = ball1.X - ball2.X
    distance = np.sqrt(np.sum(dX**2))
    if distance < ball1.size + ball2.size:
        offset = distance - (ball1.size + ball2.size)
        ball1.X += (-dX/distance)*offset/2
        ball2.X += (dX/distance)*offset/2

        dV1 = -(np.inner(ball1.V-ball2.V,ball1.X-ball2.X)/np.sum((ball1.X-ball2.X)**2))*(ball1.X-ball2.X)
        dV2 = -(np.inner(ball2.V-ball1.V,ball2.X-ball1.X)/np.sum((ball2.X-ball1.X)**2))*(ball2.X-ball1.X)
        ball1.V += dV1
        ball2.V += dV2

class Ball:
    def __init__(self, x, y, color, dx=0, dy=0, *args):
        self.X = np.asarray([np.float64(SCALING*x), np.float64(SCALING*y)])
        self.size = SCALING*1
        self.thickness = 0
        self.color = color
        self.V = np.asarray([np.float64(SCALING*dx), np.float64(-SCALING*dy)])
        #self.angle = math.atan2(self.dx, self.dy)
        #self.speed = math.hypot(self.dx, self.dy)
        self.elasticity = SCALING*dT*2e-06
        self.f = SCALING*dT*1.614e-10
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

        elif self.X[0] < self.size:
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

        if self.X[1] > height - self.size:
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

        elif self.X[1] < self.size:
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

running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    screen.fill(background_colour)
    for i, ball in enumerate(balls):
        ball.friction()
        ball.move()
        ball.bounce()
        for ball2 in balls[i+1:]:
            collide(ball, ball2)
        ball.display()
    pygame.display.flip()
    clock.tick(1000)
