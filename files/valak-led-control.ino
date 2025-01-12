#include <FastLED.h>

#define CPU_PIN 3
#define NUM_CPU_LEDS 1
#define CPU_OFFSET 0

#define UNIFAN_CENTER_LEDS 8
#define UNIFAN_SIDE_LEDS 12
#define NUM_UNIFAN_LEDS (UNIFAN_CENTER_LEDS+UNIFAN_SIDE_LEDS)

// start from front
#define BOTTOM_FANS_PIN 5
#define BOTTOM_FANS_NUM_UNIFANS 3
#define NUM_BOTTOM_FANS_LEDS (BOTTOM_FANS_NUM_UNIFANS*NUM_UNIFAN_LEDS)
#define BOTTOM_FANS_OFFSET (CPU_OFFSET+NUM_CPU_LEDS)

#define RTX_PIN 6
#define RTX_NUM_NVIDIA_LOGO_LEDS 2
#define RTX_NUM_EKWB_LOGO_LEDS 2
#define RTX_NUM_PLATE_LEDS 5
#define NUM_RTX_LEDS (RTX_NUM_NVIDIA_LOGO_LEDS+RTX_NUM_EKWB_LOGO_LEDS+RTX_NUM_PLATE_LEDS)
#define RTX_OFFSET (BOTTOM_FANS_OFFSET+NUM_BOTTOM_FANS_LEDS)

// goes from top to bottom
#define DISTROPLATE_PIN 9
#define NUM_DISTROPLATE_LEDS 27
#define DISTROPLATE_OFFSET (RTX_OFFSET+NUM_RTX_LEDS)

#define CASE_PIN 10
#define NUM_CASE_LEDS 28
#define CASE_OFFSET (DISTROPLATE_OFFSET+NUM_DISTROPLATE_LEDS)

// starts from rear
#define TOP_FANS_PIN 11
#define TOP_FANS_NUM_UNIFANS 3
#define NUM_TOP_FANS_LEDS (TOP_FANS_NUM_UNIFANS*NUM_UNIFAN_LEDS)
#define TOP_FANS_OFFSET (CASE_OFFSET+NUM_CASE_LEDS)

#define NUM_LEDS (NUM_CPU_LEDS+NUM_BOTTOM_FANS_LEDS+NUM_RTX_LEDS+NUM_DISTROPLATE_LEDS+NUM_CASE_LEDS+NUM_TOP_FANS_LEDS)

#define NUM_STRIPS 1
#define NUM_LEDS_PER_STRIP 92
#define BRIGHTNESS  255      // 255 is full brightness, 128 is half
#define MIN_BRIGHTNESS 8                   // watch the power!
#define MAX_BRIGHTNESS 255                   // watch the power!

CRGB leds[NUM_LEDS];

unsigned long previousMillis = 0;
String inputString = "";
bool stringComplete = false;

//int db[] = {2,3,1,4,0,5,8,6,7,
//            13,14,12,15,11,16,10,17,9,
//            25,24,26,23,18,22,19,21,20};
           
void setup() {
  // Serial.begin(9600);
  
  delay( 2000 ); // power-up safety delay

  //Set all lights to make sure all are working as expected
  // Assign pin 3, 5 and 6 as LED singal pin
  FastLED.addLeds<NEOPIXEL, 3>(leds, 0, NUM_CPU_LEDS);
  FastLED.addLeds<NEOPIXEL, 5>(leds, BOTTOM_FANS_OFFSET, NUM_BOTTOM_FANS_LEDS);
  FastLED.addLeds<NEOPIXEL, 6>(leds, RTX_OFFSET, NUM_RTX_LEDS);
  FastLED.addLeds<NEOPIXEL, 9>(leds, DISTROPLATE_OFFSET, NUM_DISTROPLATE_LEDS);
  FastLED.addLeds<NEOPIXEL, 10>(leds, CASE_OFFSET, NUM_CASE_LEDS);
  FastLED.addLeds<NEOPIXEL, 11>(leds, TOP_FANS_OFFSET, NUM_TOP_FANS_LEDS);

  FastLED.setBrightness( BRIGHTNESS );

  //set rainbow background
  //fill_rainbow(leds, NUM_LEDS, 0, 9);
  fill_solid(leds, NUM_LEDS, CRGB::Red);

//  for (int i = 0; i < BOTTOM_FANS_NUM_UNIFANS; i++) {
//    fill_solid(leds + BOTTOM_FANS_OFFSET + NUM_UNIFAN_LEDS * i, UNIFAN_CENTER_LEDS, CRGB::Red);
//    fill_solid(leds + BOTTOM_FANS_OFFSET + NUM_UNIFAN_LEDS * i + UNIFAN_CENTER_LEDS, UNIFAN_SIDE_LEDS, CRGB::Blue);
//  }
//    
//  for (int i = 0; i < TOP_FANS_NUM_UNIFANS; i++) {
//    fill_solid(leds + TOP_FANS_OFFSET + NUM_UNIFAN_LEDS * i, UNIFAN_CENTER_LEDS, CRGB::Red);
//    fill_solid(leds + TOP_FANS_OFFSET + NUM_UNIFAN_LEDS * i + UNIFAN_CENTER_LEDS, UNIFAN_SIDE_LEDS, CRGB::Blue);
//  }

  FastLED.show();
  delay(1000);
}

#define DELAY 500

//void serialEvent() {
//  while (Serial.available()) {
//    char inChar = (char)Serial.read();
//    inputString += inChar;
//    if (inChar == '\n') {
//      stringComplete = true;
//    }
//  }
//}

void loop() {
  
}
