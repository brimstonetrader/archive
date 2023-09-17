I'm so sorry to mussy up this section of my blog with some non-Haskell blather but today I found a use 
for python. It has long been a problem of mine that long printed documents do not fit comfortably on the
bookshelf, and I have long yearned for a method to do this, printing a book. Idk maybe if I had googled 
it better I'd have found something. What you want to do is

    > type PDF = [Page]

    > (pdfa, pdfb) = pdf.split(pdf.length() // 2)
    > pdfc = pdfa.reverse() 
    > weave(pdfc, pdfb)

    > weave :: PDF -> PDF -> PDF
    > weave [] []         = []
    > weave (a:as) (b:bs) = a:b:(weave as bs)

So you can fold the pages together and make a book. It turns out you can pull this off quick. 

    import pypdf
    pdf_in = open('book.pdf','rb')
    pdf_reader = pypdf.PdfReader(pdf_in)
    pdf_writer = pypdf.PdfWriter()
    mid = len(pdf_reader.pages) // 2
    first_half_pdf = (list(pdf_reader.pages[:mid])).reverse()
    second_half_pdf = list(pdf_reader.pages[mid:])
    for a,b in zip(first_half_pdf,second_half_pdf):
        pdf_writer.add_page(a)
        pdf_writer.add_page(b)
    pdf_out = open('new.pdf', 'wb')
    pdf_writer.write(pdf_out)
    pdf_out.close()
    pdf_in.close()

Put the file in the same directory, put 2 pages to a sheet, and be sure to flip over the short edge.
