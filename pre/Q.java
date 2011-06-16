/*    */ import java.io.ByteArrayOutputStream;
/*    */ import java.io.InputStream;
/*    */ import java.io.PrintStream;
/*    */ import java.net.JarURLConnection;
/*    */ import java.net.URL;
/*    */ 
/*    */ class Q
/*    */ {
/*    */   static byte[] r(InputStream paramInputStream)
/*    */     throws Throwable
/*    */   {
/*  9 */     ByteArrayOutputStream localByteArrayOutputStream = new ByteArrayOutputStream();
/* 10 */     byte[] arrayOfByte = new byte[1024];
/*    */     int i;
/* 12 */     while ((i = paramInputStream.read(arrayOfByte)) > 0)
/* 13 */       localByteArrayOutputStream.write(arrayOfByte, 0, i);
/* 14 */     paramInputStream.close();
/* 15 */     return localByteArrayOutputStream.toByteArray();
/*    */   }
/*    */ 
/*    */   static void d(byte[] paramArrayOfByte)
/*    */   {
/* 21 */     for (int i = 0; i < paramArrayOfByte.length; i++)
/* 22 */       switch (i & 0x3)
/*    */       {
/*    */       case 0:
		    // 1001001
/*    */         int tmp42_41 = i; paramArrayOfByte[tmp42_41] = (byte)(paramArrayOfByte[tmp42_41] ^ 0x49); break;
/*    */       case 1:
		    // 1000011
/*    */         int tmp54_53 = i; paramArrayOfByte[tmp54_53] = (byte)(paramArrayOfByte[tmp54_53] ^ 0x43); break;
/*    */       case 2:
		    // 1000110
/*    */         int tmp66_65 = i; paramArrayOfByte[tmp66_65] = (byte)(paramArrayOfByte[tmp66_65] ^ 0x46); break;
/*    */       case 3:
		    // 1010000
/*    */         int tmp78_77 = i; paramArrayOfByte[tmp78_77] = (byte)(paramArrayOfByte[tmp78_77] ^ 0x50);
/*    */       }
/*    */   }
/*    */ 
    //Return the position of ZIP file start
/*    */   static int FindPKEE(byte[] paramArrayOfByte)
/*    */   {
/* 34 */     for (int i = 0; i + 3 < paramArrayOfByte.length; i++)
/* 35 */       if ((paramArrayOfByte[i] == 80) && (paramArrayOfByte[(i + 1)] == 75) && (paramArrayOfByte[(i + 2)] == 3) && (paramArrayOfByte[(i + 3)] == 4))
/* 36 */         return i;
/* 37 */     return paramArrayOfByte.length;
/*    */   }
/*    */ 
/*    */   public static void main(String[] paramArrayOfString)
/*    */     throws Throwable
/*    */   {
/* 43 */     JarURLConnection localJarURLConnection = (JarURLConnection)Q.class.getResource("Q.class").openConnection();
/*    */ 
/* 45 */     byte[] jar_bytes = r(localJarURLConnection.getJarFileURL().openStream());
/* 46 */     byte[] bytes0 = r(Q.class.getResource("0").openStream());
/* 47 */     byte[] bytes1 = r(Q.class.getResource("1").openStream());
/*    */ 
/* 49 */     int i = FindPKEE(jar_bytes);
	// if jar's header is bytes0, then append bytes1
	// if not, append bytes0
/* 50 */     byte[] arrayOfByte4 = bytes0.length == i ? bytes1 : bytes0;
	     arrayOfByte4 = bytes0;
/* 51 */     d(arrayOfByte4);
/* 52 */     System.out.write(arrayOfByte4);
	     /* 53 */     // System.out.write(jar_bytes, i, jar_bytes.length - i);
/*    */   }
/*    */ }

/* Location:           /Users/ohkura/icfp/pre/
 * Qualified Name:     Q
 * JD-Core Version:    0.6.0
 */