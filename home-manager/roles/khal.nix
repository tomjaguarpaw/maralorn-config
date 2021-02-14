{ pkgs, ... }: {
  home = {
    packages = [ pkgs.khal ];
      #[view]
      # removed the description from the agend overview. I get a lot of stupid
      # Google ICSs files hat contain massive overhead.
      #agenda_event_format = "{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}"
      #event_format = "{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}"
    file.".config/khal/config".text = ''
      [default]
      default_calendar = Standard
      [calendars]
      [[clouds]]
      type = discover
      path = ~/.calendars/*cloud/*
      [[cda]]
      type = discover
      path = ~/.calendars/cccda/*
      [[readonly]]
      type = discover
      readonly = True
      path = ~/.calendars/*readonly/*
    '';
  };
}
