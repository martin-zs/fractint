/* Put all of the routines that call SDL audio functions in this module */

#include <stdlib.h>
#include <assert.h>
#include <SDL.h>

#include "port.h"
#include "prototyp.h"

/* SDL global variables */
SDL_AudioSpec want, have;
SDL_AudioDeviceID dev;


int SamplesPerSecond = 11025;
int ToneHz = 256;
U16 ToneVolume = 3000;
U32 RunningSampleIndex = 0;
int SquareWavePeriod;
int HalfSquareWavePeriod;
int BytesPerSample = sizeof(U16) * 2;
U32 BytesToWrite;

void *SoundBuffer;
U16 *SampleOut;
int SampleCount;


#if 1
void
SDLAudioCallback(void *UserData, Uint8 *AudioData, int Length)
{
    // Clear our audio buffer to silence.
    memset(AudioData, 0, Length);
}

void setup_sdl_audio(void)
{

memset(&want, 0, sizeof(want)); /* or SDL_zero(want) */
want.freq = 11025;
want.format = AUDIO_F32;
want.channels = 2;
want.samples = 4096;
want.callback = SDLAudioCallback;  // you wrote this function elsewhere.
dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, SDL_AUDIO_ALLOW_FORMAT_CHANGE);



}
#endif

void sdl_buzzer(void)
{

setup_sdl_audio();
SquareWavePeriod = SamplesPerSecond / ToneHz;
HalfSquareWavePeriod = SquareWavePeriod / 2;
BytesToWrite = 8000 * BytesPerSample;

SoundBuffer = malloc(BytesToWrite);
SampleOut = (U16 *)SoundBuffer;
SampleCount = BytesToWrite/BytesPerSample;


for(int SampleIndex = 0;
    SampleIndex < SampleCount;
    ++SampleIndex)
{
    U16 SampleValue = ((RunningSampleIndex++ / HalfSquareWavePeriod) % 2) ? ToneVolume : -ToneVolume;
    *SampleOut++ = SampleValue;
    *SampleOut++ = SampleValue;
}

SDL_PauseAudio(0);
SDL_QueueAudio(dev, SoundBuffer, BytesToWrite);
free(SoundBuffer);
SDL_PauseAudio(1);

}

