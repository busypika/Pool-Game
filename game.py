import pygame
import math
import numpy as np


def collide(ball1, ball2):
    dX = ball1.X - ball2.X
    distance = np.sqrt(np.sum(dX**2))
    if distance < ball1.size + ball2.size - 1:
        #tangent = math.atan2(dy, dx)
        #ball1.angle = 2*tangent - ball1.angle
        #ball2.angle = 2*tangent - ball2.angle
        #(ball1.speed, ball2.speed) = (ball2.speed, ball1.speed)

        offset = distance - (ball1.size + ball2.size)
        ball1.X += (-dX/distance)*offset/2
        ball2.X += (dX/distance)*offset/2
        print(np.sqrt(np.sum(ball1.X-ball2.X)**2))
        ball1.x = ball1.X[0]
        ball1.y = ball1.X[1]
        ball2.x = ball2.X[0]
        ball2.y = ball2.X[1]
        #print((ball1.size + ball2.size)-np.sqrt(np.sum((ball1.X-ball2.X)**2)))

        dV1 = -(np.inner(ball1.V-ball2.V,ball1.X-ball2.X)/np.sum((ball1.X-ball2.X)**2))*(ball1.X-ball2.X)
        dV2 = -(np.inner(ball2.V-ball1.V,ball2.X-ball1.X)/np.sum((ball2.X-ball1.X)**2))*(ball2.X-ball1.X)
        ball1.V += dV1
        ball2.V += dV2
        #ball1.speed = math.hypot(ball1.V[0],ball1.V[1])
        #ball2.speed = math.hypot(ball2.V[0],ball2.V[1])
        #ball1.angle = math.atan2(ball1.V[0],ball1.V[1])
        #ball2.angle = math.atan2(ball2.V[0],ball2.V[1])
        ball1.dx = ball1.V[0]
        ball1.dy = ball1.V[1]
        ball2.dx = ball2.V[0]
        ball2.dy = ball2.V[1]

        #angle = 0.5 * math.pi + tangent
        #ball1.x += math.sin(angle)*offset/2
        #ball1.y -= math.cos(angle)*offset/2
        #ball2.x -= math.sin(angle)*offset/2
        #ball2.y += math.cos(angle)*offset/2


class Ball:
    def __init__(self, x, y, color, dx=0, dy=0, *args):
        self.dt = 1000
        self.x = SCALING*x
        self.y = SCALING*y
        self.size = SCALING*1
        self.thickness = 0
        self.color = color
        self.dx = SCALING*dx*self.dt
        self.dy = SCALING*dy*self.dt
        self.angle = math.atan2(self.dx, self.dy)
        self.speed = math.hypot(self.dx, self.dy)
        self.elasticity = SCALING*self.dt*2e-06
        self.f = SCALING*self.dt*1.614e-10
        self.pos = []
        self.pos.append(self.x)
        self.pos.append(self.y)
        self.X = np.asarray([np.float64(self.x), np.float64(self.y)])
        self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])

    # Draw onto Screen
    def display(self):
        pygame.draw.circle(screen,
            self.color,
            (int(self.x), int(self.y)),  # Pygame Only Draws Integer Coordinates
            self.size,
            self.thickness)

    def move(self):
        self.X[0] += self.V[0]
        self.X[1] -= self.V[1]
        self.x = self.X[0]
        self.y = self.X[1]

    def bounce(self):
        if self.x > width - self.size:
            self.x = 2*(width - self.size) - self.x
            self.dx *= -1
            self.angle = math.atan2(self.dx, self.dy)
            self.speed -= self.elasticity
            self.dx = math.sin(self.angle) * self.speed
            self.dy = math.cos(self.angle) * self.speed
            self.X = np.asarray([np.float64(self.x), np.float64(self.y)])
            self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])

        elif self.x < self.size:
            self.x = 2*self.size - self.x
            self.dx *= -1
            self.angle = math.atan2(self.dx, self.dy)
            self.speed -= self.elasticity
            self.dx = math.sin(self.angle) * self.speed
            self.dy = math.cos(self.angle) * self.speed
            self.X = np.asarray([np.float64(self.x), np.float64(self.y)])
            self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])

        if self.y > height - self.size:
            self.y = 2*(height - self.size) - self.y
            self.dy *= -1
            self.angle = math.atan2(self.dx, self.dy)
            self.speed -= self.elasticity
            self.dx = math.sin(self.angle) * self.speed
            self.dy = math.cos(self.angle) * self.speed
            self.X = np.asarray([np.float64(self.x), np.float64(self.y)])
            self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])


        elif self.y < self.size:
            self.y = 2*self.size - self.y
            self.dy *= -1
            self.angle = math.atan2(self.dx, self.dy)
            self.speed -= self.elasticity
            self.dx = math.sin(self.angle) * self.speed
            self.dy = math.cos(self.angle) * self.speed
            self.X = np.asarray([np.float64(self.x), np.float64(self.y)])
            self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])

        if self.speed <= 0:
            self.speed = 0
            self.dx = 0
            self.dy = 0
            self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])



    def friction(self):
        self.pos.append(self.x)
        self.pos.append(self.y)
        if math.hypot((self.pos[2] - self.pos[0]), (self.pos[3] - self.pos[1])) >= 1:
            self.angle = math.atan2(self.dx, self.dy)
            self.speed = math.hypot(self.dx, self.dy)
            self.speed -= self.f
            self.dx = math.sin(self.angle) * self.speed
            self.dy = math.cos(self.angle) * self.speed
            self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])
            if self.speed <=0:
                self.speed = 0
                self.dx = 0
                self.dy = 0
                self.V = np.asarray([np.float64(self.dx), np.float64(self.dy)])
            for _ in range(4):
                self.pos.pop()
            self.pos.append(self.x)
            self.pos.append(self.y)
        else:
            for _ in range(2):
                self.pos.pop()

SCALING = 10
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
