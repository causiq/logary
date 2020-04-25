import React, { useEffect } from "react";
import useScript from "./useScript";

// DTOs
type EpochInMillis = number
type RFC3999Date = string
type DriftValue =
  | string
  | number
  | boolean
  | Date
  | EpochInMillis
  | RFC3999Date

interface DriftWidget {
  hide(): void;
  show(): void;
}
interface DriftAPI {
  widget: DriftWidget;
  setUserAttributes(attributes: Record<string, DriftValue>): void;
}

// events:
type TeamAvailability = {
  teamId: boolean;
}
type DriftData = {
  /** if your organization is currently online */
  isOnline: boolean;
  sidebarOpen: boolean;
  widgetVisible: boolean;
}
type DriftReadyPayload = {
  sidebarOpen: boolean;
  teamAvailability: TeamAvailability;
  widgetVisible: boolean;
}
type DriftReadyCallback = (api: DriftAPI, payload: DriftReadyPayload) => void

type DriftConversationBase = { conversationId: string }
type DriftConversationPayload = DriftConversationBase & {
  playbookId?: number;
  interactionId?: number;
  campaignId?: number;
}

// startConversation
type DriftStartConversationPayload = DriftConversationBase & {
  inboxId: string;
}
type DriftStartConversationCallback = (api: DriftAPI, payload: DriftStartConversationPayload) => void

// message:sent
type DriftMessageSentPayload = DriftConversationPayload & {
  inboxId: string;
}
type DriftMessageSentCallback = (payload: DriftMessageSentPayload) => void

// message
type DriftMessagePayload = DriftConversationPayload & {
  inboxId: string;
  teamMember: {
    id: number;
    name: string;
  };
}
type DriftMessageCallback = (payload: DriftMessagePayload) => void

// emailCapture
type DriftEmailCapturePayload = {
  data: DriftConversationPayload & {
    email: string;
  };
}
type DriftEmailCaptureCallback = (payload: DriftEmailCapturePayload) => void

type DriftConversationSelectedPayload = {
  conversationId: number;
  playbookId?: number;
  interactionId?: number;
  campaignId?: number;
  // no inboxId?
}
type DriftConversationSelectedCallback = (payload: DriftConversationSelectedPayload) => void

type DriftSidebarEvent = { data: DriftData }
type DriftSidebarOpenCallback = (event: DriftSidebarEvent) => void
type DriftSidebarCloseCallback = (event: DriftSidebarEvent) => void

type DriftCampaignPayload = {
  campaignId: number;
  data: DriftData;
}
type DriftCampaignCallback = (payload: DriftCampaignPayload) => void

type DriftSliderMessagePayload = {
  botMessage: string;
  userInteractedWithConversation: string;
  playbookId?: number;
  interactionId?: number;
  campaignId?: number;
}
type DriftSliderMessageCallback = (payload: DriftSliderMessagePayload) => void

/** https://devdocs.drift.com/docs/drift-events */
interface DriftEventPublisher {
  on(event: 'ready', callback: DriftReadyCallback): void;
  on(event: 'startConversation', callback: DriftStartConversationCallback): void;
  on(event: 'conversation:selected', callback: DriftConversationSelectedCallback): void;
  /** The message:sent event fires when the user replies to a conversation. */
  on(event: 'message:sent', callback: DriftMessageSentCallback): void;
  /** the message event fires when the user receives a message from a team member. */
  on(event: 'message', callback: DriftMessageCallback): void;
  on(event: 'emailCapture', callback: DriftEmailCaptureCallback): void;
  // TODO: phoneCapture
  // TODO: scheduling:requestMeeting
  // TODO: scheduling:meetingBooked
  // TODO: conversation:playbookFired
  // TODO: conversation:playbookClicked
  // TODO: conversation:playbookDismissed
  // TODO: conversation:buttonClicked
  // TODO: conversation:firstInteraction
  // TODO: gdprClicked
  on(event: 'sidebarOpen', callback: DriftSidebarOpenCallback): void;
  on(event: 'sidebarClose', callback: DriftSidebarCloseCallback): void;
  /** is the event that fires when the welcome message is open. */
  on(event: 'welcomeMessage:open', callback: () => void): void;
  /** fires when the welcome message is closed */
  on(event: 'welcomeMessage:close', callback: () => void): void;
  on(event: 'awayMessage:open', callback: () => void): void;
  on(event: 'awayMessage:close', callback: () => void): void;

  // Campaign events: The Drift widget fires events for the campaign interactions that you see in your Playbook Reports.
  // Your Playbooks are your targeted messages. They differ from your welcome message, which is the catch-all message that you've customized in your settings.
  //
  // Campaign event listeners fire for single time campaigns only (non bot-based playbooks) such as the slider or email capture messages.
  // If you are looking to listen for leadbot playbooks starting, you should use the conversation:playbookFired event described on this page.

  /** fires when campaign begins. */
  on(event: 'campaign:open', callback: DriftCampaignCallback): void;
  on(event: 'campaign:dismiss', callback: DriftCampaignCallback): void;
  on(event: 'campaign:click', callback: DriftCampaignCallback): void;
  on(event: 'campaign:submit', callback: DriftCampaignCallback): void;

  on(event: 'sliderMessage:close', callbakc: DriftSliderMessageCallback): void;
}

/**
 * Create a new Drift when the Drift script has loaded
 */
export class Drift implements DriftEventPublisher {
  inner: any;
  constructor(inst: any) {
    this.inner = inst
  }
  get api(): DriftAPI {
    return this.inner.api;
  }
  identify(userId: string, attributes?: Record<string, DriftValue>) {
    return this.inner.identify(userId, attributes)
  }
  config(...args: any) {
    return this.inner.config(...args)
  }
  track(event: string, attributes: Record<string, DriftValue>) {
    return this.inner.track(event, attributes)
  }
  reset(...args: any) {
    return this.inner.reset(...args)
  }
  debug(...args: any) {
    return this.inner.debug(...args)
  }
  show(...args: any) {
    return this.inner.show(...args)
  }
  page(page?: string) {
    return this.inner.page(page)
  }
  ping(...args: any) {
    return this.inner.ping(...args)
  }
  hide(...args: any) {
    return this.inner.hide(...args)
  }
  // @ts-ignore This shouldn't be checked since it's a multi-method and just a typing around the API Drift provides
  on(event: string, callback: (x: any) => void): void {
    return this.inner.on(event, callback)
  }
  off(...args: any) {
    return this.inner.off(...args)
  }
}

export const DriftContext = React.createContext<Drift | null>(null)

const version = '0.3.1'

const ts = 3e5, n = Math.ceil(Date.now() / ts) * ts

export default function useDrift<TRes>(appId: string, skip: boolean = false, onLoad: (drift: Drift) => TRes) {
  const src = "https://js.driftt.com/include/" + n + "/" + appId + ".js";
  const w = typeof window !== 'undefined' ? window : {} as any
  let inst = React.useRef<any>(w.drift = w.driftt = w.driftt || [])

  useEffect(() => {
    if (!inst.current.init) {
      if (inst.current.invoked) {
        const m = "Drift invoked more than once"
        console.debug && console.debug(m)
        return
      }
      inst.current.invoked = true
      inst.current.methods = ["identify", "config", "track", "reset", "debug", "show", "ping", "page", "hide", "off", "on"]
      inst.current.factory = function (method: string) {
        return function() {
          var n = Array.prototype.slice.call(arguments);
          return n.unshift(method), inst.current.push(n), inst;
        };
      }
      inst.current.methods.forEach(function (method: string) {
        inst.current[method] = inst.current.factory(method);
      })
      inst.current.SNIPPET_VERSION = version
    }
  }, [])

  return useScript(src, skip, () => {
    return onLoad(new Drift(inst.current))
  })
}