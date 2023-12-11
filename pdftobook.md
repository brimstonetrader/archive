import pypdf
pdf_in = open('aakj.pdf', 'rb')
pdf_reader = pypdf.PdfReader(pdf_in)
b = (- len(pdf_reader.pages)) % 4
mid = (len(pdf_reader.pages)-b) // 2
first_half_pdf  = reversed(pdf_reader.pages[:(mid+b)]) 
second_half_pdf = pdf_reader.pages[(mid+b):]
i = 1
pdf_writer = pypdf.PdfWriter()
# Reverse the first_half_pdf by iterating in reverse order and adding to pdf_writer
for pagea,pageb in zip(first_half_pdf,second_half_pdf):
    if i%2:
        pdf_writer.add_page(pagea)
        pdf_writer.add_page(pageb)
    else: 
        pdf_writer.add_page(pageb)
        pdf_writer.add_page(pagea)
    i += 1
i = b
for c in range(b):
    if i%2:
        pdf_writer.add_blank_page()
        pdf_writer.add_page(pdf_reader.pages[b-c-1])
    else:
        pdf_writer.add_page(pdf_reader.pages[b-c-1])
        pdf_writer.add_blank_page()
    i += 1
pdf_out = open('aakj (1).pdf', 'wb')
pdf_writer.write(pdf_out)
pdf_out.close()
pdf_in.close()
