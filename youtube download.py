from pytube import YouTube
import sys
import os
from tqdm import tqdm
import re

def get_video_id(url):
    """Extract video ID from YouTube URL"""
    pattern = r'(?:v=|\/)([0-9A-Za-z_-]{11}).*'
    match = re.search(pattern, url)
    return match.group(1) if match else None

def download_video(url, output_path=None, audio_only=False):
    """
    Download a YouTube video or audio with progress tracking and improved error handling.
    
    Args:
        url (str): YouTube video URL
        output_path (str, optional): Directory to save the file. Defaults to current directory.
        audio_only (bool, optional): If True, downloads audio only in MP3 format. Defaults to False.
    """
    try:
        # Create progress callback
        def progress_callback(stream, chunk, bytes_remaining):
            if not hasattr(progress_callback, 'pbar'):
                progress_callback.pbar = tqdm(total=stream.filesize, unit='B', unit_scale=True)
            progress_callback.pbar.update(len(chunk))
        
        # Force use of innertube API and bypass age restriction
        print("Connecting to YouTube...")
        yt = YouTube(
            url,
            on_progress_callback=progress_callback,
            use_oauth=True,
            allow_oauth_cache=True
        )
        
        # Get video details
        print(f"\nTitle: {yt.title}")
        print(f"Length: {yt.length // 60} minutes {yt.length % 60} seconds")
        print(f"Views: {yt.views:,}")
        
        if audio_only:
            # Get highest quality audio stream
            print("\nGetting available audio streams...")
            stream = yt.streams.filter(only_audio=True).first()
            if not stream:
                raise Exception("No audio streams found")
            
            # Generate safe filename for audio
            video_id = get_video_id(url)
            safe_filename = f"{video_id}_audio.mp3"
            
        else:
            # Get the highest resolution stream with both video and audio
            print("\nGetting available video streams...")
            streams = yt.streams.filter(progressive=True, file_extension='mp4')
            if not streams:
                print("No progressive streams found, trying alternative method...")
                streams = yt.streams.filter(type="video", file_extension='mp4')
            
            if not streams:
                raise Exception("No suitable streams found")
                
            stream = streams.get_highest_resolution()
            print(f"Selected quality: {stream.resolution}")
            
            # Generate safe filename for video
            video_id = get_video_id(url)
            safe_filename = f"{video_id}_{stream.resolution}.mp4"
        
        # Set output path
        if output_path is None:
            output_path = os.getcwd()
        
        # Create output directory if it doesn't exist
        os.makedirs(output_path, exist_ok=True)
        
        # Start download
        print("\nStarting download...")
        stream.download(
            output_path=output_path,
            filename=safe_filename,
            skip_existing=True
        )
        
        if hasattr(progress_callback, 'pbar'):
            progress_callback.pbar.close()
        
        print(f"\nDownload completed! File saved to: {os.path.join(output_path, safe_filename)}")
        
    except Exception as e:
        print(f"An error occurred: {str(e)}")
        print("\nTrying alternative method using yt-dlp...")
        try:
            # Alternative method using yt-dlp
            import yt_dlp
            
            ydl_opts = {
                'progress_hooks': [lambda d: print(f"\rDownloading: {d['_percent_str']} of {d['_total_bytes_str']}", end='')],
            }
            
            if audio_only:
                ydl_opts.update({
                    'format': 'bestaudio/best',
                    'postprocessors': [{
                        'key': 'FFmpegExtractAudio',
                        'preferredcodec': 'mp3',
                        'preferredquality': '192',
                    }],
                    'outtmpl': os.path.join(output_path or '', '%(id)s_audio.%(ext)s'),
                })
            else:
                ydl_opts.update({
                    'format': 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best',
                    'outtmpl': os.path.join(output_path or '', '%(id)s_%(resolution)s.%(ext)s'),
                })
            
            with yt_dlp.YoutubeDL(ydl_opts) as ydl:
                ydl.download([url])
                print(f"\nDownload completed using yt-dlp! File saved in {output_path}")
                
        except Exception as e2:
            print(f"Both methods failed. Final error: {str(e2)}")
            print("\nTroubleshooting tips:")
            print("1. Make sure you have the latest versions of the packages:")
            print("   pip install --upgrade pytube yt-dlp")
            print("2. For audio downloads, ensure you have ffmpeg installed:")
            print("   - Windows: choco install ffmpeg")
            print("   - Mac: brew install ffmpeg")
            print("   - Linux: sudo apt-get install ffmpeg")
            print("3. Check if the video is available in your region")
            print("4. Verify that the URL is correct")
            sys.exit(1)

if __name__ == "__main__":
    # First install required packages:
    # pip install pytube yt-dlp tqdm
    # Also install ffmpeg for audio conversion
    
    # Example usage
    video_url = "https://www.youtube.com/watch?v=rxhKrtb3XsE"
    output_dir = "downloads"  # Optional: specify output directory
    
    # For video (default):
    download_video(video_url, audio_only=True)
    
    # For audio only:
    # download_video(video_url, output_dir, audio_only=True)