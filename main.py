import math
import wave
import struct

from PIL import Image, ImageEnhance

def getValue(pixel):
    return (255 - pixel)/10

class component:
    def __init__(self, amplitude, frequency):
        self.amplitude = amplitude
        self.frequency = frequency
    def __repr__(self):
        return str(self.amplitude) + ":" + str(self.frequency)

class drawer:
    def __init__(self, pixels, size, x_scale, y_scale):
        self.pixels = pixels
        self.size = size
        # in Hz
        self.x_scale = x_scale
        # in milliseconds
        self.y_scale = y_scale

        self.audio = list()

        self.sample_rate = 44100.0

    def draw(self):
        width, height = self.size

        for y in range(int(height)):
            line = list()
            for x in range(width):

                value = getValue(pixels[x, y])

                Wave = component(value, x * self.x_scale)
                line.append(Wave)

            print(line)

            self.add_freq(*line)

    def add_freq(self, *waves):

        num_samples = self.y_scale * (self.sample_rate / 1000.0)

        for x in range(int(num_samples)):

            S = 0
            for wave in waves:
                S += wave.amplitude*math.cos(2 * math.pi * wave.frequency * (x / self.sample_rate))

            self.audio.append(S)

    def save_wav(self, file_name):
        wav_file = wave.open(file_name, "w")

        nchannels = 1

        sampwidth = 2

        nframes = len(self.audio)
        comptype = "NONE"
        compname = "not compressed"
        wav_file.setparams((nchannels, sampwidth, self.sample_rate, nframes, comptype, compname))

        norm = max(self.audio)

        for sample in self.audio:
            sample = sample / norm
            wav_file.writeframes(struct.pack('h', int(sample * 32767)))

        wav_file.close()

with Image.open("Sakuya[WhoAmI].jpg") as image:
    width, height = image.size

    imageGreyScale = image.convert("L")

    enhancer = ImageEnhance.Contrast(imageGreyScale)
    contrastImage = enhancer.enhance(4)

    pixels = contrastImage.load()

    Drawer = drawer(pixels, image.size, 20, 20)
    Drawer.draw()
    Drawer.save_wav("tut.wav")
    contrastImage.show()
