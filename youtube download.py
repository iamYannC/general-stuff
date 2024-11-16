from pytube import YouTube
import requests
import sys
import os
from tqdm import tqdm
import re

    ### Functions
# Extract video IDs from a YouTube playlist
def videos_from_playlist(url):
    # Get the page content
    page = requests.get(url).text
    
    # Pattern to match YouTube video IDs
    pattern = r"watch\?v=([a-zA-Z0-9_-]{11})"
    
    # Extract all matches
    video_ids = re.findall(pattern, page)
    
    # Create full URLs
    video_urls = ["https://www.youtube.com/watch?v=" + vid for vid in video_ids]
    
    return video_urls

  
# Get file name
def sanitize_filename(title):
    """
    Convert title to a safe filename by removing invalid characters.
    """
    # Remove invalid filename characters
    invalid_chars = '<>:"/\\|?*'
    filename = ''.join(char for char in title if char not in invalid_chars)
    # Replace spaces with underscores and limit length
    filename = filename.replace(' ', '_')[:100]  # Limit length to 100 chars
    return filename

# Download video
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
            
            # Generate safe filename for audio using title
            safe_filename = f"{sanitize_filename(yt.title)}.mp3"
            
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
                    'outtmpl': os.path.join(output_path or '', '%(title)s.%(ext)s'),
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


### Implementation

# Pick a playlist
playlist = "https://www.youtube.com/playlist?list=PLdUGA0NFIvcAUO-EPC4xu-1cPr4vNu_hD"

# Output directory
output_dir = r"C:\Users\97253\Documents\Software\Python\yotube\1q84"

# Get video links from a playlist
my_vids = list(set(videos_from_playlist(playlist)))

#  Download a specific video
# download_video('https://www.youtube.com/watch?v=5QLo4jBbgMg',audio_only = False)

# Loop over a list of videos
# for vid in my_vids):
#   print(f"Downloading video no. {my_vids.index(vid)+1} out of {len(my_vids)}")
#   download_video(vid, output_dir, audio_only=True)


# A better version:
for i, vid in enumerate(my_vids):
    print(f"Downloading video no. {i + 1} out of {len(my_vids)}")
    download_video(vid, output_dir, audio_only=True)
