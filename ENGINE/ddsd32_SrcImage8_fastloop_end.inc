//ddsd32_SrcImage8_fastloop_end

                                  PDWord(yy)^ := PDWord(@ppalentries[PByte(topBits)^])^;

{
                                  pEntry := @ppalentries[PByte(topBits)^];
                                  PDWord(yy)^ :=
                                    (pEntry.peRed shl 16) or
                                    (pEntry.peGreen shl 8) or
                                    (pEntry.peBlue);
}

{
                                  pEntry := @SrcImage.idx_palette[PByte(topBits)^];
                                  PDWord(yy)^ :=
                                    (pEntry.peRed shl dest_red_fmt._rshift) or
                                    (pEntry.peGreen shl dest_green_fmt._rshift) or
                                    (pEntry.peBlue shl dest_blue_fmt._rshift);
}
