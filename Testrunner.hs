module Testrunner where

runExamples :: (String -> Integer) -> [String] -> [Integer]
runExamples f examples = map f examples

compareToExampleSolutions :: (String -> Integer) -> [String] -> [Integer] -> IO()
compareToExampleSolutions f examples exampleSolutions = do
  let comparisonStrings = map makeComparisonString (makeComparison (runExamples f examples) exampleSolutions)
  putStrLn (concat comparisonStrings)

makeComparison :: [Integer] -> [Integer] -> [(Integer, Integer, Bool)]
makeComparison mySolutions exampleSolutions = map (\(mySol, exSol) -> (mySol, exSol, mySol == exSol)) a
  where a = zip mySolutions exampleSolutions

makeComparisonString :: (Integer, Integer, Bool) -> String
makeComparisonString (mySol, exSol, b)  = "Expected " ++ (show exSol) ++ ", received " ++ (show mySol) ++ s
  where s = if b then ". Correct!\n" else ". INCORRECT!\n"

--Day1
input1 = "1140\n1736\n1711\n1803\n1825\n1268\n1651\n2007\n1923\n1661\n1788\n1876\n2003\n1752\n1988\n1955\n1568\n1478\n1699\n1717\n1828\n1636\n1387\n1870\n1658\n1572\n1703\n1185\n1569\n1515\n1142\n1407\n1587\n1608\n1827\n1546\n1808\n1937\n1815\n1957\n1401\n1763\n1970\n1960\n1853\n1987\n1865\n1567\n1664\n1961\n1771\n1846\n1971\n1416\n1897\n633\n1708\n1606\n515\n1397\n1873\n1374\n1969\n1918\n1170\n1660\n1494\n1764\n2002\n1938\n1396\n1926\n1714\n1659\n1805\n1593\n1899\n1850\n1644\n1877\n1561\n1895\n1985\n1353\n395\n1919\n1522\n1745\n1721\n901\n1765\n1939\n2009\n1949\n1852\n1792\n1749\n1675\n1883\n1240\n1868\n1615\n1693\n1720\n1388\n1325\n1337\n867\n1751\n1408\n1715\n1942\n1706\n1894\n1260\n1945\n1700\n1148\n1373\n351\n1790\n1861\n1755\n1155\n1622\n1743\n1872\n1979\n1262\n1789\n1305\n1311\n1729\n1929\n823\n1623\n2005\n1932\n1814\n1909\n1728\n1592\n1712\n1363\n1338\n1804\n1402\n1198\n264\n1117\n1791\n1419\n1229\n1924\n1838\n1785\n1982\n1683\n1950\n1199\n1984\n1830\n1921\n1980\n1834\n1341\n1282\n1989\n1854\n1395\n1847\n1900\n1913\n1777\n1779\n1333\n1800\n1966\n1543\n1882\n1375\n1811\n1673\n1679\n889\n1670\n1879\n1312\n1741\n1772\n1663\n1776\n1642\n1674\n1472\n1580\n1264\n1738\n1999\n1637"
examples1 = ["1721\n979\n366\n299\n675\n1456"]
solutions1_1 = [514579]
solutions1_2 = [241861950]

--Day2
input2 = undefined
examples2 = [undefined]
solutions2 = [undefined]

--Day3
input3 = undefined
examples3 = [undefined]
solutions3 = [undefined]

--Day4
input4 = undefined
examples4 = [undefined]
solutions4 = [undefined]

--Day5
input5 = undefined
examples5 = [undefined]
solutions5 = [undefined]