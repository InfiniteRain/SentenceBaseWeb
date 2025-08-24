import * as speechSdk from "microsoft-cognitiveservices-speech-sdk";
import * as TaskPort from "elm-taskport";

const SPEECH_SYNTHESIS_CONFIG = {
  VOICE_NAME: "nl-NL-MaartenNeural",
  OUTPUT_FORMAT:
    speechSdk.SpeechSynthesisOutputFormat.Audio24Khz96KBitRateMonoMp3,
};

TaskPort.register(
  "generateAudio",
  async ({
    key,
    region,
    sentence,
  }: {
    key: string;
    region: string;
    sentence: string;
  }) => {
    const speechConfig = speechSdk.SpeechConfig.fromSubscription(key, region);

    speechConfig.speechSynthesisVoiceName = SPEECH_SYNTHESIS_CONFIG.VOICE_NAME;
    speechConfig.speechSynthesisOutputFormat =
      SPEECH_SYNTHESIS_CONFIG.OUTPUT_FORMAT;

    const synthesizer = new speechSdk.SpeechSynthesizer(speechConfig, null);

    return await new Promise((resolve, reject) => {
      synthesizer.speakTextAsync(sentence, ({ audioData, reason }) => {
        synthesizer.close();

        if (reason !== speechSdk.ResultReason.SynthesizingAudioCompleted) {
          reject(new Error("Speech SDK failure"));
          return;
        }

        const blob = new Blob([audioData], { type: "audio/mpeg" });
        const reader = new FileReader();
        reader.onloadend = () => resolve(reader.result);
        reader.readAsDataURL(blob);
      });
    });
  },
);
