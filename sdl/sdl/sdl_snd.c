/* Put all of the routines that call SDL audio functions in this module */

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <SDL.h>
#include <SDL_audio.h>
#include <SDL_platform.h>

#include "port.h"
#include "prototyp.h"

#define SAMPLE_FREQ   44100
#define Pi32 3.14159265358979f

/* SDL global variables */
SDL_AudioSpec want, have;
SDL_AudioDeviceID dev;

int SamplesPerSecond = SAMPLE_FREQ;
int ToneHz = 440;
U16 ToneVolume = 3000;
U32 RunningSampleIndex = 0;
int BytesPerSample = sizeof(U16) * 2;
U32 BytesToWrite;

//static void *SoundBuffer = NULL;
//static U16 *SampleOut = NULL;
//static int SampleCount;

static unsigned char fmtemp[9];/*temporary vars used to store value used
   to mask keyoff bit in OPL registers as they're write only and the one
   with the keyoff bit also has a frequency multiplier in it fmtemp needed
   to make sure note stays the same when in the release portion of the
   envelope*/

/* offsets of registers for each channel in the fm synth */
static unsigned char fm_offset[9] = {0,1,2,8,9,10,16,17,18};
static char fm_channel=0;/* keeps track of the channel number being used in
   polyphonic operation */
int fm_attack;
int fm_decay;
int fm_sustain;
int fm_release;
int fm_vol;
int fm_wavetype;
int polyphony;
int hi_atten;
static char offvoice = 0;

static void squarewave(void *, U32, int);
static void sinewave(void *, U32, int);

void sdl_check_for_windows(void)
{
char whichone[7];
  if (strncmp("Windows", SDL_GetPlatform(), 7) == 0)
    if (getenv("SDL_AUDIODRIVER") == NULL)
      popup_error(3, "Need Audio Driver");
  return;
}

void setup_sdl_audio(void)
{
memset(&want, 0, sizeof(want)); /* or SDL_zero(want) */
want.freq = SAMPLE_FREQ;
want.format = AUDIO_F32;
want.channels = 2;
want.samples = 4096;
want.callback = NULL;  /* Use the queue instead */

dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);

if (dev == 0)
{
   char msg[80];
   sprintf(msg, "Failed to open audio: %s", SDL_GetError());
   popup_error(3, msg);
}
else
  {
   SDL_CloseAudioDevice(dev);  /* Don't leave it open */
   dev = 0;
  }

return;
}

void cleanup_sdl_audio(void)
{
if (dev != 0)
   SDL_CloseAudioDevice(dev);

return;
}

static void sinewave(void *SineBuffer, U32 Length, int Freq)
{
int SineIndex;
int SineCount;
int SineWavePeriod;
float t, SineValue;
U16 SampleValue;
U16 *SineBuf;

SineWavePeriod = SamplesPerSecond / Freq;

SineBuf = (U16 *)SineBuffer;
SineCount = Length/BytesPerSample;

for(SineIndex = 0; SineIndex < SineCount; ++SineIndex)
  {
    t = (float)2.0 * Pi32 * SineIndex / SineWavePeriod;
    SineValue = (float)sin(t);
    SampleValue = (U16)(SineValue * ToneVolume);
    *SineBuf++ = SampleValue;
    *SineBuf++ = SampleValue;
  }
return;
}

static void squarewave(void *SquareBuffer, U32 Length, int Freq)
{
int SqrIndex;
int SqrCount;
int SquareWavePeriod;
int HalfSquareWavePeriod;
U16 SampleValue;
U16 *SqrBuf;

SquareWavePeriod = SamplesPerSecond / Freq;
HalfSquareWavePeriod = SquareWavePeriod / 2;

SqrBuf = (U16 *)SquareBuffer;
SqrCount = Length/BytesPerSample;

for(SqrIndex = 0; SqrIndex < SqrCount; ++SqrIndex)
  {
    SampleValue = ((SqrIndex / HalfSquareWavePeriod) % 2) ? ToneVolume : -ToneVolume;
    *SqrBuf++ = SampleValue;
    *SqrBuf++ = SampleValue;
  }
return;
}

void sdl_buzzer(int buzzertype)
{
U16 *BuzzPtr;

memset(&want, 0, sizeof(want)); /* or SDL_zero(want) */
want.freq = SAMPLE_FREQ;
want.format = AUDIO_F32;
want.channels = 2;
want.samples = 4096;
want.callback = NULL;  /* Use the queue instead */

dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);

if (dev != 0)
  switch(buzzertype)
  {
      case 0:
        BytesToWrite = 4096 * BytesPerSample;
        BuzzPtr = (U16 *)malloc(BytesToWrite);
        sinewave(BuzzPtr, BytesToWrite, 1047);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(100);
        SDL_PauseAudioDevice(dev, 1);
        sinewave(BuzzPtr, BytesToWrite, 1109);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(100);
        SDL_PauseAudioDevice(dev, 1);
        sinewave(BuzzPtr, BytesToWrite, 1175);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(100);
        SDL_PauseAudioDevice(dev, 1);
        break;
      case 1:
        BytesToWrite = 4096 * BytesPerSample;
        BuzzPtr = (U16 *)malloc(BytesToWrite);
        sinewave(BuzzPtr, BytesToWrite, 2093);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(100);
        SDL_PauseAudioDevice(dev, 1);
        sinewave(BuzzPtr, BytesToWrite, 1976);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(100);
        SDL_PauseAudioDevice(dev, 1);
        sinewave(BuzzPtr, BytesToWrite, 1857);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(100);
        SDL_PauseAudioDevice(dev, 1);
        break;
      default:
        BytesToWrite = 4096 * BytesPerSample * 2;
        BuzzPtr = (U16 *)malloc(BytesToWrite);
        squarewave(BuzzPtr, BytesToWrite, 40);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(500);
        SDL_PauseAudioDevice(dev, 1);
        break;
  }

if (dev != 0)
   SDL_CloseAudioDevice(dev);
dev = 0;

return;
}

void sdl_mute(void)
{
SDL_PauseAudioDevice(dev, 1);
return;
}


