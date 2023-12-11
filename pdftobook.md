I'm so sorry to mussy up this section with some non-Haskell blather but today I found a use for python.
It has long been a problem of mine that printed documents do not fit comfortably on the bookshelf, and 
I wanted a method to do this. Maybe if I had googled it better I'd have found something. Here's some 
pseudocode.

    > type PDF = [Page]                            #[1,2,3,4,5,6,7,8]

    > (pdfa, pdfb) = pdf.split(pdf.length() // 2)  #([1,2,3,4], [5,6,7,8]
    > pdfc = pdfa.reverse()                        #[4,3,2,1]
    > weave(pdfc, pdfb)                            #[4,5,6,3,2,7,8,1]

    > weave :: PDF -> PDF -> PDF
    > weave [] []         = []
    > weave (a:as) (b:bs) = a:b:(weave bs as)

If we did this to a pdf, you could fold the pages together nicely and make a book. It turns out you can 
pull this off quick in python. 

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
