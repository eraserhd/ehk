
const byte byHeader [] = { 0x02, 0x46, 0x44, 0x02};
 
int AnonymizedClassName::FindHeader(char *pData, int iLen)
{
        int iHdrIndex= 0;
        int iStartOfHdr = -1;
            
        for (int x = 0; x < iLen; x++)
        {
                for (int j = 0; j < 4; j++)
                {
			if (byHeader[j] == pData[x])
			{
				if (iStartOfHdr == -1 && pData[x] == '\x02')
					iStartOfHdr = x;
				iHdrIndex++;
				break;
			}
		}
	}
	
	if (iHdrIndex < 4)
	{
		TraceMessage(LOGLEVEL_INFORMATION,"header not found");
		// reset start of header index in case the first character found
		// coinsides with the first character of the header, but the rest
		// did not match.
		iStartOfHdr = -1;
	}
	else
	{
		if (!memcpy((char*)byHeader,pData+iStartOfHdr,4))
		{	
                        TraceMessage(LOGLEVEL_INFORMATION,"full header found starting at pos %d",iStartOfHdr);
		}
		else
		{
			TraceMessage(LOGLEVEL_INFORMATION,"header not found");
		}
	}
 
	return iStartOfHdr;
}
