import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Can submit proposal",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const wallet1 = accounts.get('wallet_1')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('proposal-registry', 'submit-proposal', [
                types.utf8("Test Proposal"),
                types.utf8("This is a test proposal for our DAO"),
                types.uint(1000000), // 1 STX
                types.utf8("Development"),
                types.uint(1),
                types.list([types.utf8("test")]),
                types.none(),
                types.list([types.utf8("milestone1")]),
                types.uint(30)
            ], wallet1.address)
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
    },
});