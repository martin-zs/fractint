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

#define SAMPLE_FREQ   22050
#define BUFFER_SIZE   2048
#define Pi32 3.14159265358979f
/* temporarily set for testing mixing function */
//#define TEST_MIX

/* SDL global variables */
SDL_AudioSpec want, have;
SDL_AudioDeviceID dev;

int SamplesPerSecond = SAMPLE_FREQ;
int ToneHz = 440;
S16 ToneVolume = 9000;
U32 RunningSampleIndex = 0;
int BytesPerSample = sizeof(S16) * 2;
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
static void MixAudio(S16 *dst, S16 *src, U32 Length);

void sdl_check_for_windows(void)
{
/* Check if using windows, then check for environment variable. */
  if (strncmp("Windows", SDL_GetPlatform(), 7) == 0)
    if (SDL_getenv("SDL_AUDIODRIVER") == NULL)
      SDL_setenv("SDL_AUDIODRIVER", "directsound", 1);
  return;
}

void setup_sdl_audio(void)
{
memset(&want, 0, sizeof(want)); /* or SDL_zero(want) */
want.freq = SAMPLE_FREQ;
want.format = AUDIO_S16;
want.channels = 2;
want.samples = BUFFER_SIZE;
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

static void MixAudio(S16 *dst, S16 *src, U32 Length)
{
/* src and dst are S16 based on AudioFormat AUDIO_S16 */
/* add src to dst and reduce volume to within range */
int MixVolume;
int MixIndex;
int MixCount;
S16 *MixBuf_src;
S16 *MixBuf_dst;

MixBuf_src = (S16 *)src;
MixBuf_dst = (S16 *)dst;
MixCount = Length / BytesPerSample;

for(MixIndex = 0; MixIndex < MixCount; ++MixIndex)
  {
//    MixVolume = (*MixBuf_dst + *MixBuf_src);
//    *MixBuf_dst++ = (S16)MixVolume;
//    *MixBuf_dst++ = (S16)MixVolume;
    *MixBuf_dst++ += *MixBuf_src++;
    *MixBuf_dst++ += *MixBuf_src++;
  }
return;
}


static void sinewave(void *SineBuffer, U32 Length, int Freq)
{
int SineIndex;
int SineCount;
int SineWavePeriod;
float t, SineValue;
S16 SampleValue;
S16 *SineBuf;

SineWavePeriod = SamplesPerSecond / Freq;

SineBuf = (S16 *)SineBuffer;
SineCount = Length/BytesPerSample;

for(SineIndex = 0; SineIndex < SineCount; ++SineIndex)
  {
    t = (float)2.0 * Pi32 * SineIndex / SineWavePeriod;
    SineValue = (float)sin(t);
    SampleValue = (S16)(SineValue * ToneVolume);
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
S16 SampleValue;
S16 *SqrBuf;

SquareWavePeriod = SamplesPerSecond / Freq;
HalfSquareWavePeriod = SquareWavePeriod / 2;

SqrBuf = (S16 *)SquareBuffer;
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
S16 *BuzzPtr;
S16 *BuzzPtr_1;
S16 *BuzzPtr_2;

memset(&want, 0, sizeof(want)); /* or SDL_zero(want) */
want.freq = SAMPLE_FREQ;
want.format = AUDIO_S16;
want.channels = 2;
want.samples = BUFFER_SIZE;
want.callback = NULL;  /* Use the queue instead */

dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);

if (dev != 0)
  switch(buzzertype)
  {
#ifdef TEST_MIX
      case 0:
        BytesToWrite = BUFFER_SIZE * BytesPerSample;
        BuzzPtr = (S16 *)malloc(BytesToWrite);
        BuzzPtr_1 = (S16 *)malloc(BytesToWrite);
        BuzzPtr_2 = (S16 *)malloc(BytesToWrite);
        sinewave(BuzzPtr_1, BytesToWrite, 1047);
//        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
//        SDL_PauseAudioDevice(dev, 0);
//        SDL_Delay(100);
//        SDL_PauseAudioDevice(dev, 1);
//        squarewave(BuzzPtr_2, BytesToWrite, 40);
        sinewave(BuzzPtr_2, BytesToWrite, 1109);
        MixAudio(BuzzPtr_1, BuzzPtr_2, BytesToWrite);
        SDL_QueueAudio(dev, BuzzPtr_1, BytesToWrite);
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
        break;
      default:
        break;
#else
      case 0:
        BytesToWrite = BUFFER_SIZE * BytesPerSample;
        BuzzPtr = (S16 *)malloc(BytesToWrite);
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
        BytesToWrite = BUFFER_SIZE * BytesPerSample;
        BuzzPtr = (S16 *)malloc(BytesToWrite);
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
        BytesToWrite = BUFFER_SIZE * BytesPerSample * 2;
        BuzzPtr = (S16 *)malloc(BytesToWrite);
        squarewave(BuzzPtr, BytesToWrite, 40);
        SDL_QueueAudio(dev, BuzzPtr, BytesToWrite);
        SDL_PauseAudioDevice(dev, 0);
        SDL_Delay(500);
        SDL_PauseAudioDevice(dev, 1);
        break;
#endif // TEST_MIX
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

/*
; ****************** Function buzzer(int buzzertype) *******************
;
;       Sound a tone based on the value of the parameter
;
;       0 = normal completion of task
;       1 = interrupted task
;       2 = error condition

;       "buzzer()" codes:  strings of two-word pairs
;               (frequency in cycles/sec, delay in milliseconds)
;               frequency == 0 means no sound
;               delay     == 0 means end-of-tune
*/
void buzzer(int buzzertype)
{
  sdl_buzzer(buzzertype);
  return;
}

/*
; ************** Function tone(int frequency,int delaytime) **************
;
;       buzzes the speaker with this frequency for this amount of time
*/
void tone(int frequency, int delaytime)
{
}

/*
; ************** Function snd(int hertz) and nosnd() **************
;
;       turn the speaker on with this frequency (snd) or off (nosnd)
;
; *****************************************************************
*/
int soundon(int hertz)
{
  return(0);
}

void soundoff(void)
{
  return;
}

void mute(void)
{
  sdl_mute();
  return;
}


