# Spectrum drawer

A small utility to translate images (jpg, png, etc.) to wave format.

## Usage
To build the binary run command below in the project directory.

```
cabal build
```

Programm has next command line options

```
./spectrumDrawer path/to/picture [name.wav] [dt] [df]
```
Where
* `dt` - time step length, by default equals 0.05 s;
* `dt` - frequency step, by default equals 250 Hz.  

To run with the `cabal`

```
cabal v2-run ./spectrumDrawer.cabal path/to/image [name.wav] [dt] [df]
```

## Limitations

Due to the laziness of Haskell current implementation requires an enormous amount of memory for rendering the wave file. So, even a small image with 112x160 can take up to 7 Gb of RAM. Thus, I would recommend limiting image size up to around 120x180.

## Example
Let's take image with enough low definition 112x160.

![origin](images/pic_origin.jpg)

It would be quite helpful to increase its contrast, making separating lines clearer.

![high contrast](images/pic.png)

By default Audacity shows the spectrum with vertical line as Frequency axes. So it will look better there if we rotate it 90°.

![rotated](images/pic_rotated.png)

Then run the command below

```
./spectrumDrawer ./pic_rotated.png audio.wav 0.1 250
```

So we will obtain the new file `audio.wav`, which spectrogram, we can check in Audacity

![spectrum](images\spectrum.png)
