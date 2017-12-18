{-# LANGUAGE Rank2Types #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , getWalletAddrs
    , getWalletAddrsDetector
    , decodeCTypeOrFail
    , getWalletAssuredDepth
    ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as S
import           Formatting                 (build, sformat, (%))

import           Pos.Core                   (BlockCount)
import           Pos.Util.Servant           (FromCType (..), OriginType)
import           Pos.Util.Util              (maybeThrow)
import           Pos.Wallet.Web.Assurance   (AssuranceLevel (HighAssurance),
                                             assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal, cwAssurance)


import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode, CurrentAndRemoved (..),
                                             NeedSorting (..), WebWalletModeDB,
                                             getAccountAddrMaps, getAccountIds,
                                             getAccountWAddresses, getWalletMeta)

getWalletAccountIds :: WebWalletModeDB ctx m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWId) <$> getAccountIds

getAccountAddrsOrThrow
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> NeedSorting -> AccountId -> m [CWAddressMeta]
getAccountAddrsOrThrow mode needSort accId =
    getAccountWAddresses mode needSort accId >>= maybeThrow noWallet
  where
    noWallet =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getWalletAddrMetas
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetas lookupMode cWalId =
    concatMapM (getAccountAddrsOrThrow lookupMode (NeedSorting False)) =<<
    getWalletAccountIds cWalId

getWalletAddrs
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs mode wid = cwamId <<$>> getWalletAddrMetas mode wid

getWalletAddrsDetector
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m (CId Addr -> Bool)
getWalletAddrsDetector lookupMode cWalId = do
    accIds <- getWalletAccountIds cWalId
    case accIds of
        [accId] -> do
            -- can avoid conversion to Set
            addrMaps <- getAccountAddrMaps accId
            return $ \addr ->
                any (HM.member addr) [getCurrent addrMaps, getRemoved addrMaps]

        _ -> do
            addrMetas <- concatMapM (getAccountAddrsOrThrow lookupMode (NeedSorting False)) accIds
            let addrSet = S.fromList $ map cwamId addrMetas
            return $ flip S.member addrSet

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either (throwM . DecodeError) pure . decodeCType

getWalletAssuredDepth
    :: (WebWalletModeDB ctx m)
    => CId Wal -> m (Maybe BlockCount)
getWalletAssuredDepth wid =
    assuredBlockDepth HighAssurance . cwAssurance <<$>>
    getWalletMeta wid
