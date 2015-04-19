#load "TapTrain.fsx"
#r @"../../packages/FSharp.Collections.ParallelSeq.1.0.2\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "../CA.fs"
#load "../CAUtils.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS.fs"
#load "../CARunner.fs"
open CAUtils
open Navigation
open CA
let tocfg (p:Parm array) =
    
        {
            xyz_accel_quite_limit                   = vF32 p.[0] // 0.5f
            TapConfig = 
                    {
                        gstr_time_limit             = vI64 p.[1] //100000000L

                        y_accel_front_thrld         = vF32 p.[2] //1.7f
                        y_accel_back_thrld          = vF32 p.[3] //1.7f
                        y_accel_avg_front_thrld     = vF32 p.[4] //1.5f
                        y_accel_avg_back_thrld      = vF32 p.[5] //-1.5f
                        avg_over_count              = vF32 p.[6] //4.0f

                        xz_accel_tolerance          = vF32 p.[7] //0.5f

                        xy_rot_tolerance            = vF32 p.[8] //1.0f
                        z_rot_tolerance             = vF32 p.[9] //2.7f

                        TapCfg =
                            {
                                gstr_time_limit                 = vI64 p.[10] //700000000L

                                time_limit_to_reach_low_accel   = vI64 p.[11] //400000000L
                                low_y_accel_limit               = vF32 p.[12] //0.4f

                                x_accel_tolerance               = vF32 p.[13] //2.1f
                                z_accel_tolerance               = vF32 p.[14] //2.1f

                                ret_x_accel_tolerance           = vF32 p.[15] //3.1f
                                ret_z_accel_tolerance           = vF32 p.[16] //2.1f

                                xy_rot_tolerance                = vF32 p.[17] //1.0f
                                z_rot_tolerance                 = vF32 p.[18] //2.5f

                            }
                    }
            LRConfig = 
                    {
                        x_grav_high_limit       = vF32 p.[19] //8.5f
                        x_grav_low_limit        = vF32 p.[20] //2.0f
        
                        z_grav_zero_tolerance   = vF32 p.[21] //3.0f
                        z_grav_Left_thrsld      = vF32 p.[22] //-4.0f
                        z_grav_right_thrsld     = vF32 p.[23] //6.f
                    }
            SwipeConfig =
                    {
                        gstr_time_limit             = vI64 p.[24] //100000000L

                        z_accel_front_thrsld        = vF32 p.[25] //1.7f
                        z_accel_back_thrsld         = vF32 p.[26] //-1.7f
                        z_accel_avg_front_thrsld    = vF32 p.[27] //1.5f
                        z_accel_avg_back_thrsld     = vF32 p.[28] //-1.5f

                        xy_accel_tolerance          = vF32 p.[29] //0.5f

                        avg_over_count              = vF32 p.[30] //4.0f

                        xz_rot_tolerance            = vF32 p.[31] //0.1f
                        y_rot_tolerance             = vF32 p.[32] //2.5f

                        SwipeCfg =
                            {
                                gstr_time_limit         = vI64 p.[33] //600000000L

                                x_accel_tolerance       = vF32 p.[34] //2.1f
                                y_accel_tolerance       = vF32 p.[35] //2.1f

                                ret_x_accel_tolerance   = vF32 p.[36] //2.1f
                                ret_y_accel_tolerance   = vF32 p.[37] //3.1f

                                xz_rot_tolerance        = vF32 p.[38] //0.1f
                                y_rot_tolerance         = vF32 p.[39] //2.5f

                                low_z_accel_limit       = vF32 p.[40] //0.3f
                            }
                    }
        }

let parms =
    [|
        F32(0.f,0.1f,2.f)      // xyz_accel_quite_limit  = vF32 p.[0] // 0.5f
//            TapConfig = 
//                    {
        I64(0L, 1000000L,1000000000L) //gstr_time_limit  = vI64 p.[1] //100,000,000L

        F32(0.f,0.1f,5.f) // y_accel_front_thrld         = vF32 p.[2] //1.7f
        F32(0.f,0.1f,5.f) // y_accel_back_thrld          = vF32 p.[3] //1.7f
        F32(0.f,0.1f,5.f) // y_accel_avg_front_thrld     = vF32 p.[4] //1.5f
        F32(0.f,0.1f,5.f) // y_accel_avg_back_thrld      = vF32 p.[5] //-1.5f
        I(0,1,10)         // avg_over_count              = vF32 p.[6] //4.0f

        F32(0.f,0.1f,5.f) // xz_accel_tolerance          = vF32 p.[7] //0.5f

        F32(0.f,0.1f,5.f) // xy_rot_tolerance            = vF32 p.[8] //1.0f
        F32(0.f,0.1f,5.f) // z_rot_tolerance             = vF32 p.[9] //2.7f

//                        TapCfg =
//                            {
        I64(0L, 1000000L,1000000000L) // gstr_time_limit                 = vI64 p.[10] //700000000L

        I64(0L, 1000000L,1000000000L) // time_limit_to_reach_low_accel   = vI64 p.[11] //400000000L
        F32(0.f,0.1f,5.f)             // low_y_accel_limit               = vF32 p.[12] //0.4f

        F32(0.f,0.1f,5.f)             // x_accel_tolerance               = vF32 p.[13] //2.1f
        F32(0.f,0.1f,5.f)             // z_accel_tolerance               = vF32 p.[14] //2.1f

        F32(0.f,0.1f,5.f)             // ret_x_accel_tolerance           = vF32 p.[15] //3.1f
        F32(0.f,0.1f,5.f)             // ret_z_accel_tolerance           = vF32 p.[16] //2.1f

        F32(0.f,0.1f,5.f)             // xy_rot_tolerance                = vF32 p.[17] //1.0f
        F32(0.f,0.1f,5.f)             // z_rot_tolerance                 = vF32 p.[18] //2.5f

//                            }
//                    }
//            LRConfig = 
//                    {
        F32(0.f,0.1f,5.f)             // x_grav_high_limit       = vF32 p.[19] //8.5f
        F32(0.f,0.1f,5.f)             // x_grav_low_limit        = vF32 p.[20] //2.0f
        
        F32(0.f,0.1f,5.f)             // z_grav_zero_tolerance   = vF32 p.[21] //3.0f
        F32(0.f,-5.f,-0.1f)           // z_grav_Left_thrsld      = vF32 p.[22] //-4.0f
        F32(0.f,0.f,8.f)              // z_grav_right_thrsld     = vF32 p.[23] //6.f
//                    }
//            SwipeConfig =
//                    {
        I64(0L, 1000000L,1000000000L) // gstr_time_limit         = vI64 p.[24] //100000000L

        F32(0.f,1.f,5.f)             // z_accel_front_thrsld     = vF32 p.[25] //1.7f
        F32(0.f,-5.f,-1.f)           // z_accel_back_thrsld      = vF32 p.[26] //-1.7f
        F32(0.f,1.f,5.f)             // z_accel_avg_front_thrsld = vF32 p.[27] //1.5f
        F32(0.f,-5.f,-1.f)           // z_accel_avg_back_thrsld  = vF32 p.[28] //-1.5f

        F32(0.f,0.1f,5.f)           // xy_accel_tolerance        = vF32 p.[29] //0.5f

        I(0,1,10)                   // avg_over_count            = vF32 p.[30] //4.0f

        F32(0.f,0.1f,5.f)           // xz_rot_tolerance          = vF32 p.[31] //0.1f
        F32(0.f,0.1f,5.f)           // y_rot_tolerance           = vF32 p.[32] //2.5f

//                        SwipeCfg =
//                            {
        I64(0L, 1000000L,1000000000L) // gstr_time_limit         = vI64 p.[33] //600000000L

        F32(0.f,0.1f,5.f)             // x_accel_tolerance       = vF32 p.[34] //2.1f
        F32(0.f,0.1f,5.f)             // y_accel_tolerance       = vF32 p.[35] //2.1f

        F32(0.f,0.1f,5.f)             // ret_x_accel_tolerance   = vF32 p.[36] //2.1f
        F32(0.f,0.1f,5.f)             // ret_y_accel_tolerance   = vF32 p.[37] //3.1f

        F32(0.f,0.1f,5.f)             // xz_rot_tolerance        = vF32 p.[38] //0.1f
        F32(0.f,0.1f,5.f)             // y_rot_tolerance         = vF32 p.[39] //2.5f

        F32(0.f,0.1f,5.f)             // low_z_accel_limit       = vF32 p.[40] //0.3f
//                            }
//                    }
//        }
    |]
