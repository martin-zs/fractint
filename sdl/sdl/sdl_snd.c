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
#include "helpdefs.h"

#define SAMPLE_FREQ   44100
#define BUFFER_SIZE   2048
#ifndef M_PI
  #define M_PI 3.14159265358979323846
#endif

/* SDL global variables */
SDL_AudioSpec want, have;
SDL_AudioDeviceID dev;

int SamplesPerSecond = SAMPLE_FREQ;
S16 ToneVolume = 5000;
U32 RunningSampleIndex = 0;
int BytesPerSample;
U32 BytesToWrite;
int menu2;

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

static void tone(int, int);
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
BytesToWrite = have.size;  // NO LONGER NEED THIS
BytesPerSample = (int)(have.size / have.samples);

return;
}

void cleanup_sdl_audio(void)
{
if (dev != 0)
   SDL_CloseAudioDevice(dev);
dev = 0;

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
float temp;
S16 SampleValue;
S16 *SineBuf;

SineBuf = (S16 *)SineBuffer;
SineCount = Length/BytesPerSample;
temp = (float)2.0 * M_PI * (float)Freq / (float)SamplesPerSecond;

for(SineIndex = 0; SineIndex < SineCount; ++SineIndex)
  {
    SampleValue = (S16)(ToneVolume * (float)sin(temp * SineIndex));
    *SineBuf++ = SampleValue;
    *SineBuf++ = SampleValue;
  }
//  SampleValue = (S16)(ToneVolume * (float)sin(temp));
//  *SineBuf++ = SampleValue;
//  *SineBuf++ = SampleValue;
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

if (soundflag & 7)
  {
  if (dev == 0)
    dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);

#ifndef XFRACT
  SDL_Delay(500); /* Delay so windows audio device stabilizes */
#endif // XFRACT

  if (dev != 0)
    switch(buzzertype)
    {
      case 0:
        tone(1047, 100);
        tone(1109, 100);
        tone(1175, 100);
        break;
      case 1:
        tone(2093, 100);
        tone(1976, 100);
        tone(1857, 100);
        break;
      case 2:
      default:
        tone(40, 500);
        break;
    }

  SDL_PauseAudioDevice(dev, 1);
  SDL_ClearQueuedAudio(dev);
  SDL_CloseAudioDevice(dev);
  dev = 0;
  }

return;
}

/*
; ************** Function tone(int frequency,int delaytime) **************
;
;       Buzzes the speaker with this frequency for this amount of time
;       (in milliseconds).
;       The audio device (dev) is opened and closed in the routine that
;       calls this function.
*/
static void tone(int frequency, int delaytime)
{
  double timeticks; /* in seconds */
  double cycle_fraction;    /* addition fraction of a cycle to send to audio device */
  int BufSize = BytesPerSample * SamplesPerSecond * 0.1; /* 1 tenth second */
  void *BuzzPtr;

  if (delaytime < 1)
     delaytime = 1;
  if (frequency != 0) /* If frequency == 0, only do the delay */
     {
      SDL_PauseAudioDevice(dev, 1);             /* Pause audio device */
//      timeticks = (double)(frequency * (double)delaytime / 1000.0);
//      timeticks -= (int) timeticks;
//      cycle_fraction =  (1.0 - timeticks) / (double)frequency;
//cycle_fraction = 0;
//      BufSize = BytesPerSample * (int)(SamplesPerSecond * (cycle_fraction + ((double)delaytime / 1000.0)));
      BuzzPtr = (void *)malloc(BufSize);
memset(BuzzPtr, 0, BufSize);
      squarewave(BuzzPtr, BufSize, frequency);
      SDL_QueueAudio(dev, BuzzPtr, BufSize);
//      SDL_Delay(1);                             /* Wait 1 ms for queue to fill */
      free(BuzzPtr);
      SDL_PauseAudioDevice(dev, 0);             /* Unpause audio device */
     }

  sleepms(delaytime);
//  SDL_Delay(delaytime);
//  wait_until(0, delaytime);
//  SDL_PauseAudioDevice(dev, 1);             /* Pause audio device */
//  SDL_ClearQueuedAudio(dev);

  return;
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
/* Returns a 1 if sound is turned on, a 0 if not. */

/* clip to 5 Khz to match the limits set in asm routine that drives pc speaker*/
  if (hertz > 5000)
     return (0);
/* and get rid of really silly bass notes too */
  if (hertz < 20)
     return (0);

/* make sure the audio device is open */
  if (dev == 0)
     dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);

//  tone(hertz, 100);
  tone(hertz, orbit_delay);

  return(1);
}

void soundoff(void)
{
  SDL_PauseAudioDevice(dev, 1);
  SDL_ClearQueuedAudio(dev);
#if 0
  if (dev != 0)
     {
      SDL_CloseAudioDevice(dev);
      dev = 0;
     }
#endif
  return;
}

void mute(void)
{
  SDL_PauseAudioDevice(dev, 1);

  return;
}

#define SYNTH_IMPLEMENTED  0 /* Change to 1 when implemented */

#define LOADCHOICES(X)     {\
   static FCODE tmp[] = { X };\
   strcpy(ptr,(char *)tmp);\
   choices[++k]= ptr;\
   ptr += sizeof(tmp);\
   }

int get_sound_params(void)
{
/* routine to get sound settings  */
static FCODE o_hdg[] = {"Sound Control Screen"};
char *soundmodes[] = {s_off,s_beep,s_x,s_y,s_z};
int old_soundflag,old_orbit_delay;
char hdg[sizeof(o_hdg)];
char *choices[15];
char *ptr;
struct fullscreenvalues uvalues[15];
int k;
int i;
int oldhelpmode;
char old_start_showorbit;

oldhelpmode = helpmode;
old_soundflag = soundflag;
old_orbit_delay = orbit_delay;
old_start_showorbit = start_showorbit;

/* soundflag bits 0..7 used as thus:
   bit 0,1,2 controls sound beep/off and x,y,z
      (0 == off 1 == beep, 2 == x, 3 == y, 4 == z)
   bit 3 controls PC speaker
   bit 4 controls sound card OPL3 FM sound
   bit 5 controls midi output
   bit 6 controls pitch quantise
   bit 7 free! */
get_sound_restart:
   menu2 = 0;
   k = -1;
   strcpy(hdg,o_hdg);
   ptr = (char *)extraseg;

   LOADCHOICES("Sound (off, beep, x, y, z)");
   uvalues[k].type = 'l';
   uvalues[k].uval.ch.vlen = 4;
   uvalues[k].uval.ch.llen = 5;
   uvalues[k].uval.ch.list = soundmodes;
   uvalues[k].uval.ch.val = soundflag&7;
#if SYNTH_IMPLEMENTED
   LOADCHOICES("Use PC internal speaker?");
   uvalues[k].type = 'y';
   uvalues[k].uval.ch.val = (soundflag & 8)?1:0;

   LOADCHOICES("Use soundcard output?");
   uvalues[k].type = 'y';
   uvalues[k].uval.ch.val = (soundflag & 16)?1:0;
/*
   LOADCHOICES("Midi...not implemented yet");
   uvalues[k].type = 'y';
   uvalues[k].uval.ch.val = (soundflag & 32)?1:0;
*/
   LOADCHOICES("Quantize note pitch ?");
   uvalues[k].type = 'y';
   uvalues[k].uval.ch.val = (soundflag & 64)?1:0;
#endif
   LOADCHOICES("Orbit delay in ms (0 = none)");
   uvalues[k].type = 'i';
   uvalues[k].uval.ival = orbit_delay;

   LOADCHOICES("Base Hz Value");
   uvalues[k].type = 'i';
   uvalues[k].uval.ival = basehertz;

   LOADCHOICES("Show orbits?");
   uvalues[k].type = 'y';
   uvalues[k].uval.ch.val = start_showorbit;

   LOADCHOICES("");
   uvalues[k].type = '*';
#if SYNTH_IMPLEMENTED
   LOADCHOICES("Press F6 for FM synth parameters, F7 for scale mappings");
   uvalues[k].type = '*';
#endif
   LOADCHOICES("Press F4 to reset to default values");
   uvalues[k].type = '*';

   oldhelpmode = helpmode;
   helpmode = HELPSOUND;
   i = fullscreen_prompt(hdg,k+1,choices,uvalues,255,NULL);
   helpmode = oldhelpmode;
   if (i <0) {
      soundflag = old_soundflag;
      orbit_delay = old_orbit_delay;
      start_showorbit = old_start_showorbit;
      return(-1); /*escaped */
   }

   k = -1;

   soundflag = uvalues[++k].uval.ch.val;
#if SYNTH_IMPLEMENTED
   soundflag = soundflag + (uvalues[++k].uval.ch.val * 8);
   soundflag = soundflag + (uvalues[++k].uval.ch.val * 16);
 /*  soundflag = soundflag + (uvalues[++k].uval.ch.val * 32); */
   soundflag = soundflag + (uvalues[++k].uval.ch.val * 64);
#endif
   orbit_delay = uvalues[++k].uval.ival;
   basehertz = uvalues[++k].uval.ival;
   start_showorbit = (char)uvalues[++k].uval.ch.val;
#if SYNTH_IMPLEMENTED
   /* now do any intialization needed and check for soundcard */
   if ((soundflag & 16) && !(old_soundflag & 16)) {
     initfm();
   }

   if (i == F6) {
      get_music_parms();/* see below, for controling fmsynth */
      goto get_sound_restart;
   }

   if (i == F7) {
      get_scale_map();/* see below, for setting scale mapping */
      goto get_sound_restart;
   }
#endif
   if (i == F4) {
      soundflag = 1; /* reset to default */
      orbit_delay = 0;
      basehertz = 440;
      start_showorbit = 0;
      goto get_sound_restart;
   }

   if (soundflag != old_soundflag && ((soundflag&7) > 1 || (old_soundflag&7) > 1))
      return (1);
   else
      return (0);
}


