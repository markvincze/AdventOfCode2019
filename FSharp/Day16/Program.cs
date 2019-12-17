using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Day16
{
    class Program
    {
        private static int GetMultiplier(int index, int number)
        {
            switch ((((index + 1) / (number + 1)) % 4))
            {
                case 0: return 0;
                case 1: return 1;
                case 2: return 0;
                case 3: return -1;
                default: throw new InvalidOperationException();
            }
        }

        private static void CalcPhase(int[] input, int[] output, int offset)
        {
            Parallel.For(0, 1000, chunk =>
            {
                var upperLimit = chunk < 99 ? (chunk + 1) * (input.Length / 100) : input.Length;
                for (int i = chunk * (input.Length / 100); i < upperLimit; i++)
                {
                    var digit = 0;
                    // int j = i; // Start from i, the rest can be skipped
                    int j = Math.Max(offset, i); // Start from i or the offset, the rest from the beginning can be skipped
                    while (j < input.Length)
                    {
                        digit += input[j] * GetMultiplier(j, i);

                        var mod = (j + 1) % ((i + 1) * 2);
                        if (i > 0 && mod == ((i + 1) * 2) - 1)
                        {
                            j += (i + 2);
                        }
                        else
                        {
                            j += 1;
                        }
                    }
                    output[i] = (Math.Abs(digit) % 10);
                }
            });

            // for (int i = 0; i < input.Length; i++)
            // {
            //     var digit = 0;
            //     int j = i; // Start from j, the rest can be skipped
            //     while (j < input.Length)
            //     {
            //         digit += input[j] * GetMultiplier(j, i);

            //         var mod = (j + 1) % ((i + 1) * 2);
            //         if (i > 0 && mod == ((i + 1) * 2) - 1)
            //         {
            //             j += (i + 2);
            //         }
            //         else
            //         {
            //             j += 1;
            //         }
            //     }
            //     output[i] = (Math.Abs(digit) % 10);
            // }
        }

        private static int[] CalcPhaseN(int n, int[] digits1, int offset)
        {
            var digits2 = new int[digits1.Length];

            for (int i = 0; i < n; i++)
            {
                Console.WriteLine("Calculating Phase {0}", i);
                CalcPhase(digits1, digits2, offset);
                var tmp = digits1;
                digits1 = digits2;
                digits2 = tmp;
            }

            return digits1;
        }

        static void Main(string[] args)
        {
            // var input1 = "12345678".Select(c => Int32.Parse(c.ToString())).ToArray();
            // var input = "80871224585914546619083218645595".Select(c => Int32.Parse(c.ToString())).ToArray();
            // var input1 = "59738571488265718089358904960114455280973585922664604231570733151978336391124265667937788506879073944958411270241510791284757734034790319100185375919394328222644897570527214451044757312242600574353568346245764353769293536616467729923693209336874623429206418395498129094105619169880166958902855461622600841062466017030859476352821921910265996487329020467621714808665711053916709619048510429655689461607438170767108694118419011350540476627272614676919542728299869247813713586665464823624393342098676116916475052995741277706794475619032833146441996338192744444491539626122725710939892200153464936225009531836069741189390642278774113797883240104687033645".Select(c => Int32.Parse(c.ToString())).ToArray();
            var input1 = "03036732577212944063491565474664".Select(c => Int32.Parse(c.ToString())).ToArray();

            // var output1 = new int[input1.Length];
            // CalcPhase(input1, output1);
            // Console.WriteLine(
            //     "Result1: {0}",
            //     String.Concat(output1.Take(8).Select(i => i.ToString())));

            // 5973857
            // 6500000

            var output1 = CalcPhaseN(100, input1, 0);

            Console.WriteLine(
                "Result1: {0}",
                String.Concat(output1.Take(8).Select(i => i.ToString())));

            input1 = "03036732577212944063491565474664".Select(c => Int32.Parse(c.ToString())).ToArray();
            var messageOffset = Int32.Parse(String.Concat(input1.Take(7).Select(d => d.ToString())));

            var input2 = new int[input1.Length * 10000];

            for (int i = 0; i < 10000; i++)
            {
                for (int j = 0; j < input1.Length; j++)
                {
                    try
                    {
                        input2[i * input1.Length + j] = input1[j];
                    }
                    catch (IndexOutOfRangeException ex)
                    {
                        Console.WriteLine("Exception, input2.Length: {0}, i: {1}, j: {2}", input2.Length, i, j);
                    }
                }
            }

            var output2 = CalcPhaseN(100, input2, messageOffset);

            Console.WriteLine("Message offset: {0}, Output length: {1}", messageOffset, output2.Length);

            Console.WriteLine(
                "Result2: {0}",
                String.Concat(output2.Skip(messageOffset).Take(8).Select(i => i.ToString())));
        }
    }
}
