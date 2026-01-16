#!/usr/bin/env python3
"""
Rotate PDF files in Funder_raw_bryophyte_datasheets by 90 degrees clockwise,
excluding files in SKJELLINGAHAUGEN folder.
"""

import os
import sys
from pathlib import Path

# Try to import pypdf, install if not available
try:
    from pypdf import PdfReader, PdfWriter
except ImportError:
    print("Installing pypdf...")
    os.system(f"{sys.executable} -m pip install pypdf --user --quiet")
    from pypdf import PdfReader, PdfWriter

def rotate_pdf(input_path, output_path, rotation=90):
    """
    Rotate a PDF file by specified degrees (90 = clockwise, -90 = counterclockwise).
    """
    try:
        reader = PdfReader(input_path)
        writer = PdfWriter()
        
        for page in reader.pages:
            # Rotate 90 degrees clockwise (to the right)
            page.rotate(rotation)
            writer.add_page(page)
        
        with open(output_path, 'wb') as output_file:
            writer.write(output_file)
        
        return True
    except Exception as e:
        print(f"Error rotating {input_path}: {e}")
        return False

def main():
    base_dir = Path("Funder_raw_bryophyte_datasheets")
    
    if not base_dir.exists():
        print(f"Error: Directory {base_dir} not found!")
        return
    
    # Find all PDF files, excluding SKJELLINGAHAUGEN
    pdf_files = []
    for pdf_file in base_dir.rglob("*.pdf"):
        # Skip SKJELLINGAHAUGEN folder
        if "SKJELLINGAHAUGEN" in str(pdf_file):
            print(f"Skipping (already rotated): {pdf_file}")
            continue
        pdf_files.append(pdf_file)
    
    print(f"Found {len(pdf_files)} PDF files to rotate (excluding SKJELLINGAHAUGEN)")
    
    # Rotate each PDF
    success_count = 0
    for pdf_file in pdf_files:
        print(f"Rotating: {pdf_file}")
        
        # Create a temporary file name
        temp_file = pdf_file.with_suffix('.tmp.pdf')
        
        # Rotate the PDF
        if rotate_pdf(pdf_file, temp_file, rotation=90):
            # Replace original with rotated version
            pdf_file.unlink()  # Delete original
            temp_file.rename(pdf_file)  # Rename temp to original
            success_count += 1
            print(f"  ✓ Successfully rotated")
        else:
            # Clean up temp file if rotation failed
            if temp_file.exists():
                temp_file.unlink()
            print(f"  ✗ Failed to rotate")
    
    print(f"\nCompleted: {success_count}/{len(pdf_files)} files rotated successfully")

def rotate_gudmedalen_back():
    """
    Rotate GUDMEDALEN PDF files back 90 degrees (counterclockwise)
    to undo the previous rotation.
    """
    base_dir = Path("Funder_raw_bryophyte_datasheets")
    
    if not base_dir.exists():
        print(f"Error: Directory {base_dir} not found!")
        return
    
    # Find all PDF files in GUDMEDALEN folder
    pdf_files = []
    for pdf_file in base_dir.rglob("*.pdf"):
        if "GUDMEDALEN" in str(pdf_file):
            pdf_files.append(pdf_file)
    
    print(f"Found {len(pdf_files)} PDF files in GUDMEDALEN to rotate back")
    
    # Rotate each PDF -90 degrees (counterclockwise)
    success_count = 0
    for pdf_file in pdf_files:
        print(f"Rotating back: {pdf_file}")
        
        # Create a temporary file name
        temp_file = pdf_file.with_suffix('.tmp.pdf')
        
        # Rotate the PDF -90 degrees (counterclockwise)
        if rotate_pdf(pdf_file, temp_file, rotation=-90):
            # Replace original with rotated version
            pdf_file.unlink()  # Delete original
            temp_file.rename(pdf_file)  # Rename temp to original
            success_count += 1
            print(f"  ✓ Successfully rotated back")
        else:
            # Clean up temp file if rotation failed
            if temp_file.exists():
                temp_file.unlink()
            print(f"  ✗ Failed to rotate")
    
    print(f"\nCompleted: {success_count}/{len(pdf_files)} files rotated back successfully")

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == "gudmedalen":
        rotate_gudmedalen_back()
    else:
        main()
