unit Download;

{
  Inno Setup
  Copyright (C) 1997-2012 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Download function
}

interface

uses
  DownloadForm;

var
  WebDownloadForm: TWebDownloadForm;

procedure DownloadWebFile(const WebFilename, Description, DestFilename, Referer,
  ProxyUserName, ProxyPassword: String; IgnoreUnknownCA: Boolean;
  FtpTextMode, FtpPassive: Boolean; WebSetupDownload: Boolean);

implementation

uses
  Windows, Classes, Install, Logging, Main, Msgs, MsgIDs, PathFunc, SetupTypes,
  SysUtils, Wizard, WebDownloader, CmnFunc2;

procedure UpdateWebDownloadStatus(Downloader: TWebDownloader);
var
  Status: String;
  RemainingTime: string;
  NewPercentage: Integer;
begin
  Status := '';
  case Downloader.Status of
    wdsConnecting: Status := SetupMessages[msgDownloadConnecting];
    wdsDisconnecting: Status := SetupMessages[msgDownloadDisconnecting];
    wdsDownloading: Status := SetupMessages[msgDownloadDownloading];
  end;
  if Downloader.Status = wdsDownloading then begin
    if WebDownloadForm = nil then begin
      if Downloader.TotalSize = 0 then begin
        Status := FmtSetupMessage(msgDownloadElapsed, [Status,
          IntToStr(Downloader.BytesRead div 1024), Downloader.GetElapsedTimeStr]);
      end
      else begin
        RemainingTime := Downloader.GetRemainingTimeStr;
        if RemainingTime <> '' then
          Status := FmtSetupMessage(msgDownloadRemaining, [Status, RemainingTime]);
        NewPercentage := (Downloader.BytesRead * 100 div Downloader.TotalSize);
        if NewPercentage <> WizardForm.DownloadProgressGauge.Position then
          WizardForm.DownloadProgressGauge.Position := NewPercentage;
      end;
    end
    else
      WebDownloadForm.UpdateProgress(Status, Downloader.BytesRead, Downloader.TotalSize);
  end;

  if WebDownloadForm = nil then begin
    WizardForm.DownloadStatusLabel.Caption := Status;
    WizardForm.DownloadStatusLabel.Visible := True;
    WizardForm.DownloadFilenameLabel.Visible := True;
    WizardForm.DownloadProgressGauge.Visible := True;
  end
  else
    WebDownloadForm.StatusLabel.Caption := Status;
end;

procedure URLDownloadWebFile(const URL, Description, DestFilename, Referer,
  ProxyUserName, ProxyPassword: String; IgnoreUnknownCA: Boolean;
  FtpTextMode, FtpPassive: Boolean; WebSetupDownload: Boolean);
var
  Downloader: TWebDownloader;
  Stream: TFileStream;
  ProtDomain: string;
  I: Integer;
  OwnDownloadDlg: Boolean;
begin
  LogFmt('Downloading "%s" to "%s"', [URL, DestFilename]);

  I := Pos('://', URL) + 3;
  while (I <= Length(URL)) and (URL[I] <> '/') and (URL[I] <> '\') and
         (URL[I] <> ':') and (URL[I] <> '@') and (URL[I] <> '?') do
    Inc(I);
  ProtDomain := Copy(URL, 1, I - 1);

  OwnDownloadDlg := False;
  Downloader := TWebDownloader.Create;
  try
    try
      Stream := TFileStream.Create(DestFilename, fmCreate);
      try
        { Initialize WebDownload-Progress }
        if not WebSetupDownload and (WizardForm.CurPageID <> wpInstalling) then begin
          { No WebSetupDownload means no download dialog unless the current page is the installation
            page. So create an own download dialog. }
          WebSetupDownload := True;
          OwnDownloadDlg := True;
          WebDownloadForm := TWebDownloadForm.Create(nil);
          WebDownloadForm.AllSize := High(Smallint);
          if InstallMode = imNormal then
            WebDownloadForm.Show;
        end;

        if WebSetupDownload then begin
          if Description <> '' then
            WebDownloadForm.FilenameLabel.Caption := Description
          else
            WebDownloadForm.FilenameLabel.Caption := ProtDomain + '   ' + PathExtractName(DestFilename);
        end
        else begin
          WizardForm.DownloadProgressGauge.Position := 0;
          WizardForm.DownloadProgressGauge.Min := 0;
          WizardForm.DownloadProgressGauge.Max := 100;
          if Description <> '' then
            WizardForm.DownloadFilenameLabel.Caption := Description
          else
            WizardForm.DownloadFilenameLabel.Caption := ProtDomain + '   ' + PathExtractName(DestFilename);
        end;

        { Initialize Downloader }
        if SetupHeader.AppVerName <> '' then
          Downloader.Agent := 'Inno Setup - ' + SetupHeader.AppVerName
        else
          Downloader.Agent := 'Inno Setup';
        if WebSetupDownload or (WizardForm.InstallingPage.Notebook.ActivePage = WizardForm.InstallingPage) then
          Downloader.OnProcessEvents := ProcessEvents;
        Downloader.OnStatus := UpdateWebDownloadStatus;
        Downloader.OnProgress := UpdateWebDownloadStatus;
        Downloader.Referer := Referer;
        Downloader.FtpTextMode := FtpTextMode;
        Downloader.FtpPassive := FtpPassive;
        Downloader.ProxyUserName := ProxyUserName;
        Downloader.ProxyPassword := ProxyPassword;
        if WebSetupDownload then
          Downloader.Timeout := 10000;
        Downloader.IgnoreUnknownCA := IgnoreUnknownCA;

        if not Assigned(Downloader.OnProcessEvents) then
          WizardForm.Enabled := False;
        try
          { Download file }
          Downloader.DownloadFile(URL, Stream);
          if Downloader.Status = wdsError then begin
            if Downloader.ErrorText = '' then
              raise Exception.Create(FmtSetupMessage1(msgDownloadFailed, URL))
            else
              raise Exception.Create(Downloader.ErrorText);
          end;
        finally
          if not Assigned(Downloader.OnProcessEvents) then
            WizardForm.Enabled := True;
        end;
      finally
        if not WebSetupDownload then begin
          { Clean up GUI }
          WizardForm.DownloadStatusLabel.Visible := False;
          WizardForm.DownloadStatusLabel.Caption := '';
          WizardForm.DownloadFilenameLabel.Visible := False;
          WizardForm.DownloadFilenameLabel.Caption := '';
          WizardForm.DownloadProgressGauge.Visible := False;
        end;
        Stream.Free;
      end;
    except
      Log('Deleting temporary download file');
      DeleteFile(DestFilename);
      raise;
    end;
  finally
    if OwnDownloadDlg then
      FreeAndNil(WebDownloadForm);
    Downloader.Free;
  end;
end;

procedure DownloadWebFile(const WebFilename, Description, DestFilename, Referer,
  ProxyUserName, ProxyPassword: String; IgnoreUnknownCA: Boolean;
  FtpTextMode, FtpPassive: Boolean; WebSetupDownload: Boolean);
var
  I: Integer;
  Protocol, Location: String;
  URL: String;
  UrlList: TStrings;
  Index: Integer;
  Success: Boolean;
begin
  UrlList := TStringList.Create;
  try
    { Split multiple URLs }
    URL := WebFilename + ' ';
    I := Pos(' ', URL);
    while I <> 0 do begin
      if I > 1 then
        UrlList.Add(Trim(Copy(URL, 1, I - 1)));
      URL := Trim(Copy(URL, I + 4, Length(URL)));
      I := Pos(' ', URL);
    end;
    if UrlList.Count = 0 then
      raise Exception.Create('UrlList.Count = 0');

    Success := False;
    repeat
      Index := Random(UrlList.Count);
      URL := UrlList[Index];
      UrlList.Delete(Index);

      I := Pos('://', URL);
      Protocol := Copy(URL, 1, I - 1);
      Location := Copy(URL, I + 3, Length(WebFilename));
      try
        { User defined download function }
        if CompareText(Protocol, 'download') = 0 then begin
          if (CodeRunner <> nil) then
            CodeRunner.RunProcedure('WebFileDownloadHandler', [Location, Description, DestFilename], True);
        end
        else
        if (CompareText(Protocol, 'http') = 0) or
           (CompareText(Protocol, 'https') = 0) or
           (CompareText(Protocol, 'ftp') = 0) then begin
          { HTTP / HTTPS / FTP }
          URLDownloadWebFile(URL, Description, DestFilename, Referer,
            ProxyUserName, ProxyPassword, IgnoreUnknownCA,
            FtpTextMode, FtpPassive, WebSetupDownload);
        end
        else
          raise Exception.Create(FmtSetupMessage1(msgDownloadUnknownProtocol, Protocol));

        if NeedToAbortInstall then
          Abort;

        Success := True;
      except
        on EAbort do
          raise;
        on E: Exception do begin
          { Try another URL if there are alternatives }
          if UrlList.Count > 0 then
            Success := False
          else
            raise;
        end;
      end;
    until Success;
  finally
    UrlList.Free;
  end;
end;

end.
